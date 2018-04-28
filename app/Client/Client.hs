{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.Client where

import Control.Lens
import Control.Monad

import Data.Aeson
import Data.Aeson.Lens (key, nth, _Object)
import Data.Aeson.Types
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

isWork :: String -> IO Bool
isWork jobName = do
    let url = "http://localhost:3000/retrieve/" ++ jobName ++ "/exists"
    response <- get url

    pure $ fromMaybe False $ read . Char8.unpack <$> response ^? responseBody

getWork :: String -> Int -> IO (Int, Map String String)
getWork jobName workerId = do
    let url = "http://localhost:3000/retrieve/" ++ jobName ++ "/data"
    response <- asValue =<< get url

    pure $ fromMaybe (error "Failed to decode response.") $
        flip parseMaybe (response ^. responseBody) $ \(Object obj) -> do
            taskId <- obj .: "taskId"
            d <- obj .: "data"

            pure (taskId, d)

reportStatus :: Int -> Int -> String -> IO ()
reportStatus taskId workerId status = do
    let url = "http://localhost:3000/report/" ++ show taskId ++ "/" ++ show workerId ++ "/status"
    post url $ partString "status" status

    -- TODO: Handle an error.
    pure ()

runJob :: String -> IO ()
runJob job = do
    curPath <- getCurrentDirectory

    -- We want to do everything in a separate directory.
    createDirectoryIfMissing True job
    setCurrentDirectory job

    workerId <- newWorker job
    putStrLn $ "Got worker id: " ++ show workerId

    let workingPath = "worker-" ++ show workerId
    createDirectoryIfMissing True workingPath
    setCurrentDirectory workingPath

    putStrLn "Looking for config.yaml"
    whenM (not <$> doesPathExist "config.yaml") $ do
        contents <- requestConfig job
        writeFile "config.yaml" contents

    config <- readConfig "config.yaml"

    putStrLn "Beginning runner loop."

    if config ^. reusePaths then do
        workConfig <- initStructure config
        runnerLoopNoSetup workConfig workerId
        pure ()
    else
        runnerLoop config workerId

    setCurrentDirectory curPath

    where
        runnerLoopNoSetup config workerId = do
            workExists <- isWork (config ^. jobName)

            if workExists then do
                curPath <- getCurrentDirectory

                let workingPath = getWorkingPath config

                setCurrentDirectory workingPath

                (taskId, jobData) <- getWork (config ^. jobName) workerId
                putStrLn $ "Running task: " ++ show taskId

                let resultPaths = getResultPaths config

                curFiles <- foldl Map.union Map.empty <$> mapM (listFiles False) resultPaths

                -- Report that we started it and then run the commands
                reportStatus taskId workerId "running"
                (exitCode,_,_) <- runSysCommands jobData (config ^. runner)

                -- Check for new files, if that's a thing we do.
                newFiles <- foldl Map.union Map.empty <$> mapM (listFiles False) resultPaths

                when (config ^. diffUpload) $ do
                    let diff = Map.differenceWith (\cur new -> if cur >= new then Nothing else Just new) curFiles newFiles
                    let diffKeys = Map.keys diff ++ (Map.keys newFiles \\ Map.keys curFiles) -- Make sure to include new files as well as modified files.

                    -- Upload new/modified files.
                    mapM_ (\p -> whenM (doesFileExist p) $ do
                                    putStrLn $ "Uploading: " ++ p
                                    uploadFile (config ^. jobName) taskId p) diffKeys

                setCurrentDirectory curPath

                if config ^. reusePaths then
                    case exitCode of
                        ExitSuccess -> do
                            reportStatus taskId workerId "finished"
                            runnerLoopNoSetup config workerId
                        ExitFailure _ -> do
                            -- TODO: Look at the exit code and send it.
                            reportStatus taskId workerId "failed"
                            runnerLoopNoSetup config workerId
                else
                    pure (exitCode, taskId)
            else
                pure (ExitSuccess, -1)

        runnerLoop config workerId =
            whenM (isWork (config ^. jobName)) $ do
                -- Create the directory structure.
                workConfig <- initStructure config

                (exitCode, taskId) <- runnerLoopNoSetup workConfig workerId

                case exitCode of
                    ExitSuccess -> do
                        reportStatus taskId workerId "finished"
                        runnerLoop config workerId
                    ExitFailure _ -> do
                        -- TODO: Look at the exit code and send it.
                        reportStatus taskId workerId "failed"
                        runnerLoop config workerId

-----------------------------------------------
-- These functions can only be run on the server where the PostgreSQL server is hosted.
----------------------------------------------
initJob :: FilePath -> IO ()
initJob configPath = do
    putStrLn "Reading configuration file."
    rawConfig <- readFile configPath
    config <- readConfig configPath

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

    putStrLn $ "Submitting job: " ++ (config ^. jobName)
    runFunction "select jobs_insert(?, ?)" (config ^. jobName, rawConfig) :: IO [Only ()]

    putStrLn $ "Submitting " ++ show (length taskData) ++ " tasks."
    mapM_ (insertTask config) taskData

    putStrLn $ "Setting current directory back to: " ++ curPath
    setCurrentDirectory curPath

    pure ()

    where
        insertTask :: Config -> [(String, String)] -> IO [Only ()]
        insertTask config entry = runFunction "select tasks_insert(?, ?, ?)" (config ^. jobName, toArray $ map fst entry, toArray $ map snd entry)

