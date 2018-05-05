{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Monad.IO.Class

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Data.Aeson ((.=), object)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import Web.Scotty

import Database

server :: Int -> IO ()
server p = scotty p $ do
    get "/retrieve/:job/:workerId/data" $ do
        job <- param "job" :: ActionM T.Text
        workerId <- param "workerId" :: ActionM Int
        taskIds <- liftIO (runFunction "select tasks_get_next(?, ?)" (job, workerId) :: IO [Only Integer])

        case taskIds of
            (Only taskId:_) -> do
                dataItems <- liftIO (runFunction "select * from task_data_get(?)" (Only taskId) :: IO [(String, String)])
                json $ object ["taskId" .= taskId, "data" .= Map.fromList dataItems]

            -- Return empty response so the client knows that there is no more work to do.
            [] -> text ""

    get "/retrieve/:job/new-worker/:name" $ do
        job <- param "job" :: ActionM String
        name <- param "name" :: ActionM String

        [Only worker_id] <- liftIO (runFunction "select workers_insert(?, ?)" (name, job) :: IO [Only Integer])
        text $ LT.pack $ show worker_id

    get "/retrieve/:job/request-file/:filename" $ do
        job <- param "job" :: ActionM String
        fileName <- param "filename" :: ActionM String

        [Only fileContents] <- liftIO (runFunction "select job_files_get(?, ?)" (job, fileName) :: IO [Only B.ByteString])

        raw fileContents

    get "/retrieve/:job/config" $ do
        job <- param "job" :: ActionM String

        [Only fileContents] <- liftIO (runFunction "select configs_get(?)" (Only job) :: IO [Only B.ByteString])

        raw fileContents

    get "/report/:workerId/aborted" $ do
        workerId <- read <$> param "workerId" :: ActionM Int

        liftIO (runFunction "select tasks_aborted(?)" (Only workerId) :: IO [Only ()])

        pure ()

    get "/list" $ do
        results <- liftIO (runFunction_ "select jobs_list()" :: IO [(T.Text, Integer)])

        json $ object $ map (uncurry (.=)) results

    post "/report/:id/:workerId/status" $ do
        status <- param "status" :: ActionM String
        taskId <- read <$> param "id" :: ActionM Int
        workerId <- read <$> param "workerId" :: ActionM Int

        liftIO (runFunction "select task_log_insert(?, ?, ?)" (taskId, workerId, status) :: IO [Only ()])

        pure ()

    post "/submit/:job/:taskId/file" $ do
        job <- param "job" :: ActionM String
        taskId <- read <$> param "taskId" :: ActionM Int
        fileName <- param "filename" :: ActionM String
        fileContents <- param "file" :: ActionM B.ByteString

        liftIO (runFunction "select result_files_insert(?, ?, ?, ?)" (job, taskId, fileName, fileContents) :: IO [Only ()])

        pure ()

