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

import Args
import Client.Files
import Client.Types
import Database
import Process
import Types
import Util

-- | Performs a request, meant to be used with Network.Wreq request functions like `get` and `post`.
-- Provide the path relative to the host and port specified in the args (in the WorkerState).
doRequest :: (MonadIO m, MonadState WorkerState m) =>
             (String -> IO (Response B.ByteString)) ->
             String ->
             m (Response B.ByteString)
doRequest requester path = do
    h <- view (args.host) <$> State.get
    p <- view (args.port) <$> State.get

    liftIO $ requester $ "http://" ++ h ++ ":" ++ show p ++ path

newWorker :: (MonadIO m, MonadState WorkerState m) => m Int
newWorker = do
    job <- view (args.job) <$> State.get
    hostName <- liftIO getHostName

    response <- doRequest get $ "/retrieve/" ++ job ++ "/new-worker/" ++ hostName

    newId <-
        case read . Char8.unpack <$> response ^? responseBody of
            -- TODO: Better error handling.
            Nothing -> error $ show response
            Just i -> pure i

    State.modify $ set workerId newId
    pure newId

getWork :: (MonadIO m, MonadState WorkerState m) => m (Maybe (Int, Map String String))
getWork = do
    job <- view (args.job) <$> State.get
    workerId <- view workerId <$> State.get

    baseR <- doRequest get $ "/retrieve/" ++ job ++ "/" ++ show workerId ++ "/data"

    if B.null (baseR ^. responseBody) then
        pure Nothing
    else do
        response <- liftIO $ asValue baseR

        pure $ flip parseMaybe (response ^. responseBody) $ \(Object obj) -> do
                taskId <- obj .: "taskId"
                d <- obj .: "data"

                pure (taskId, d)

reportStatus :: (MonadIO m, MonadState WorkerState m) => Int -> String -> m ()
reportStatus taskId status = do
    workerId <- view workerId <$> State.get

    doRequest (`post` partString "status" status) $ "/report/" ++ show taskId ++ "/" ++ show workerId ++ "/status"

    -- TODO: Handle an error.
    pure ()

doWork :: (MonadIO m, MonadState WorkerState m) => m (Maybe (Int, ExitCode))
doWork = do
    workingPath <- getWorkingPath . view config <$> State.get
    job <- view (config.jobName) <$> State.get

    taskInfo <- getWork
    case taskInfo of
        Nothing -> do
            liftIO $ putStrLn $ "No more work to do for '" ++ job ++ "'"
            pure Nothing
        Just (taskId, jobData) -> do
            reportStatus taskId "running"
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

setupRunner :: (MonadIO m, MonadState WorkerState m) => m ()
setupRunner = do
    job <- view (args.job) <$> State.get

    liftIO $ createDirectoryIfMissing True job

    baseConfig <- liftIO $ setupJob job
    State.modify $ set config baseConfig

    newWorker
    wid <- view workerId <$> State.get
    liftIO $ putStrLn $ "Got worker id: " ++ show wid

    let workingPath = job ++ "/worker-" ++ show wid
    liftIO $ createDirectoryIfMissing True workingPath

    liftIO $ putStrLn "Beginning runner loop."
    -- If we reuse the paths, then just set up everything once.
    when (baseConfig ^. reusePaths) $ do
        workConfig <- initStructure
        State.modify $ set config workConfig

    runnerLoop

    where
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
                            reportStatus taskId "finished"
                            runnerLoop
                        ExitFailure _ -> do
                            -- TODO: Look at the exit code and send it.
                            reportStatus taskId "failed"
                            runnerLoop

runJob :: Args -> IO ()
runJob args = do
    -- Need to do this because onException works with IO, not MonadIO
    let blankState = WorkerState (-1) "." args emptyConfig
    (_, newState) <- State.runStateT newWorker blankState
    let wid = view workerId newState

    flip onException (abort newState) $ do
        State.runStateT setupRunner newState

        pure ()

    where
        abort s = do
            flip State.runStateT s $ do
                workerId <- view workerId <$> State.get
                liftIO $ putStrLn "Aborting current task..."
                doRequest get $ "/report/" ++ show workerId ++ "/aborted"

                -- TODO: Handle an error while aborting.

            pure ()

listJobs :: Args -> IO ()
listJobs = print

-----------------------------------------------
-- These functions can only be run on the server where the PostgreSQL server is hosted.
----------------------------------------------
submitJobConfig :: Args -> IO Config
submitJobConfig args = do
    let configPath = args ^. job
    putStrLn "Reading configuration file."
    rawConfig <- readFile configPath
    config <- readConfig configPath

    putStrLn $ "Submitting job, if necessary (will not duplicate): " ++ (config ^. jobName)
    runFunction "select jobs_insert(?)" (Only (config ^. jobName)) :: IO [Only ()]

    putStrLn $ "Submitting configuration for version: " ++ (config ^. jobVersion)
    runFunction "select configs_insert(?, ?, ?, ?)" (config ^. jobName, config ^. jobVersion, rawConfig, config ^. timeout) :: IO [Only ()]

    pure config

submitJob :: Args -> IO ()
submitJob args = do
    let configPath = args ^. job -- Because job is the second argument passed to `amphi submit`
    config <- submitJobConfig args

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

