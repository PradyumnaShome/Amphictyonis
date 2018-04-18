{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import Control.Lens
import Control.Monad

import Data.Aeson
import Data.Aeson.Lens (key, nth, _Object)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromMaybe)

import Network.HostName
import Network.Wreq

import System.Directory

whenM :: Monad m => m Bool -> m () -> m ()
whenM v a = v >>= (`when` a)

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

    let obj = response ^. responseBody . key "json" . _Object

    case (,) <$> (fromJSON <$> HashMap.lookup "taskId" obj)
             <*> (fromJSON <$> HashMap.lookup "data" obj) of
        Just (Success taskId, Success d) -> pure (taskId, d)
        _ -> error "Failed" -- TODO: Better error handling.

runner :: String -> FilePath -> IO ()
runner jobName scriptName = do
    workerId <- newWorker jobName
    putStrLn $ "Got worker id: " ++ show workerId

    let workingPath = "worker-" ++ show workerId
    whenM (doesDirectoryExist workingPath) $ removeDirectoryRecursive workingPath
    createDirectory workingPath
    putStrLn $ "Cleared out and created directory: " ++ workingPath

    putStrLn "Beginning runner loop."
    runnerLoop workerId

    where
        runnerLoop workerId =
            whenM (isWork jobName) $ do
                (taskId, jobData) <- getWork jobName workerId
                -- report "running" taskId workerId
                pure ()

createNewJob :: String -> IO ()
createNewJob jobName = do
    let url = "http://localhost:3000/submit/new-job"
    post url $ partString "jobName" jobName

    -- TODO: Deal with this when it fails.
    pure ()

-- while amphi retrieve --job sample --exists
-- do
--     id=$(amphi retrieve sample --next-id)
--     args=$(amphi retrieve --job sample --id "$id" --data)

--     report="amphi report --job sample --id \"$id\" --worker \"$worker_id\""

--     $report -s "running"
--     bash runner.sh $args

--     if [[ "$?" -eq "0" ]]; then
--         $report -s "finished"
--     else
--         $report -s "failed"
--     fi
-- done

