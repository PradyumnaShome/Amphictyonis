{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Client.Client where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State

import Data.Aeson
import Data.Aeson.Lens (key, nth, _Object)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromMaybe)
import Data.Time.Clock

import Database.PostgreSQL.Simple

import Network.HostName
import Network.URI
import Network.Wreq

import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Client.Files
import Client.Types
import Database
import Process
import Types
import Util

newWorker :: String -> IO Int
newWorker job = do
    hostName <- getHostName

    let url = "http://localhost:3000/retrieve/" ++ job ++ "/new-worker/" ++ hostName
    response <- get url

    case read . Char8.unpack <$> response ^? responseBody of
        -- TODO: Better error handling.
        Nothing -> error $ show response
        Just i -> pure i

getWork :: String -> Int -> IO (Maybe (Int, Map String String))
getWork jobName workerId = do
    let url = "http://localhost:3000/retrieve/" ++ jobName ++ "/" ++ show workerId ++ "/data"
    baseR <- get url

    if B.null (baseR ^. responseBody) then
        pure Nothing
    else do
        response <- asValue baseR

        pure $ flip parseMaybe (response ^. responseBody) $ \(Object obj) -> do
                taskId <- obj .: "taskId"
                d <- obj .: "data"

                pure (taskId, d)

reportStatus :: Int -> Int -> String -> IO ()
reportStatus taskId workerId status = do
    let url = "http://localhost:3000/report/" ++ show taskId ++ "/" ++ show workerId ++ "/status"
    post url $ partString "status" status

    -- TODO: Handle an error.
    pure ()

doWork :: (MonadIO m, MonadState WorkerState m) => m (Maybe (Int, ExitCode))
doWork = do
    workingPath <- getWorkingPath . view config <$> State.get
    job <- view (config . jobName) <$> State.get
    wid <- view workerId <$> State.get

    taskInfo <- liftIO $ getWork job wid
    case taskInfo of
        Nothing -> do
            liftIO $ putStrLn $ "No more work to do for '" ++ job ++ "'"
            pure Nothing
        Just (taskId, jobData) -> do
            liftIO $ reportStatus taskId wid "running"
            liftIO $ putStrLn $ "Running task: " ++ show taskId

            resultPaths <- getResultPaths . view config <$> State.get

            curFiles <- liftIO $ foldl Map.union Map.empty <$> mapM (listFiles False) resultPaths

            -- Report that we started it and then run the commands
            runnerScript <- view (config . runner) <$> State.get
            (exitCode,_,_) <- liftIO $ runSysCommands workingPath jobData runnerScript

            -- Check for new files, if that's a thing we do.
            newFiles <- liftIO $ foldl Map.union Map.empty <$> mapM (listFiles False) resultPaths

            whenM (view (config . diffUpload) <$> State.get) $ liftIO $ do
                let diff = Map.differenceWith (\cur new -> if cur >= new then Nothing else Just new) curFiles newFiles
                let diffKeys = Map.keys diff ++ (Map.keys newFiles \\ Map.keys curFiles) -- Make sure to include new files as well as modified files.

                -- Upload new/modified files.
                mapM_ (\p -> whenM (doesFileExist p) $ do
                                putStrLn $ "Uploading: " ++ p
                                uploadFile job taskId p) diffKeys

            pure $ Just (taskId, exitCode)

setupJob :: String -> IO Config
setupJob job = do
    putStrLn "Getting latest config.yaml."
    contents <- requestConfig job
    writeFile (job ++ "/config.yaml") contents

    readConfig (job ++ "/config.yaml")

runJob :: String -> IO ()
runJob job = do
    createDirectoryIfMissing True job

    baseConfig <- setupJob job

    workerId <- newWorker job
    putStrLn $ "Got worker id: " ++ show workerId

    let workingPath = job ++ "/worker-" ++ show workerId
    createDirectoryIfMissing True workingPath

    putStrLn "Beginning runner loop."

    flip onException (abort workerId) $ do
        -- If we reuse the paths, then just set up everything once.
        if baseConfig ^. reusePaths then
            State.runStateT (initStructure >>= (\workConfig -> do
                State.modify $ set config workConfig
                runnerLoop)) $ WorkerState workerId workingPath baseConfig
        else
            State.runStateT runnerLoop $ WorkerState workerId workingPath baseConfig

        pure ()

    where
        abort workerId = do
            putStrLn "Aborting current task..."
            let url = "http://localhost:3000/report/" ++ show workerId ++ "/aborted"
            get url

            -- TODO: Handle an error.
            pure ()

        runnerLoop = do
            baseConfig <- view config <$> State.get
            curWorkerId <- view workerId <$> State.get

            curConfig <-
                if not (baseConfig ^. reusePaths) then
                    initStructure
                else
                    pure baseConfig

            result <- doWork

            case result of
                Nothing -> pure ()
                Just (taskId, exitCode) ->
                    case exitCode of
                        ExitSuccess -> do
                            liftIO $ reportStatus taskId curWorkerId "finished"
                            runnerLoop
                        ExitFailure _ -> do
                            -- TODO: Look at the exit code and send it.
                            liftIO $ reportStatus taskId curWorkerId "failed"
                            runnerLoop

-----------------------------------------------
-- These functions can only be run on the server where the PostgreSQL server is hosted.
----------------------------------------------
submitJobConfig :: FilePath -> IO Config
submitJobConfig configPath = do
    putStrLn "Reading configuration file."
    rawConfig <- readFile configPath
    config <- readConfig configPath

    putStrLn $ "Submitting job, if necessary (will not duplicate): " ++ (config ^. jobName)
    runFunction "select jobs_insert(?)" (Only (config ^. jobName)) :: IO [Only ()]

    putStrLn $ "Submitting configuration for version: " ++ (config ^. jobVersion)
    runFunction "select configs_insert(?, ?, ?, ?)" (config ^. jobName, config ^. jobVersion, rawConfig, config ^. timeout) :: IO [Only ()]

    pure config

submitJob :: FilePath -> IO ()
submitJob configPath = do
    config <- submitJobConfig configPath

    let configDir = takeDirectory configPath

    curPath <- getCurrentDirectory
    putStrLn $ "Changing directory to: " ++ configDir
    setCurrentDirectory configDir

    putStrLn $ "Running data generation for " ++ (config ^. jobName)

    results <- concat <$> mapM sysCommand (config ^. dataSources)

    putStrLn "Read output is:"
    let readTaskData = readCSV results
    -- Filter out entries that don't match the schema of the first row.
    let expectedKeys = map fst $ head readTaskData
    let taskData = filter (\r -> map fst r == expectedKeys) readTaskData
    print taskData

    putStrLn $ "Submitting " ++ show (length taskData) ++ " tasks."
    mapM_ (insertTask config) taskData

    putStrLn $ "Setting current directory back to: " ++ curPath
    setCurrentDirectory curPath

    pure ()

    where
        insertTask :: Config -> [(String, String)] -> IO [Only ()]
        insertTask config entry = runFunction "select tasks_insert(?, ?, ?)" (config ^. jobName, toArray $ map fst entry, toArray $ map snd entry)

