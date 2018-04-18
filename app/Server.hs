{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Monad.IO.Class

import Database.PostgreSQL.Simple

import Data.Maybe (isJust)
import Data.Monoid (mconcat)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text.Lazy as T

import Web.Scotty

import Database

server :: IO ()
server = scotty 3000 $ do
    get "/retrieve/:job/data" $ do
        job <- param "job" :: ActionM T.Text
        [Only taskId] <- liftIO (runFunction "select tasks_get_next(?)" (Only job) :: IO [Only (Maybe Int)])
        dataItems <- liftIO (runFunction "select task_data_get(?)" (Only taskId) :: IO [(String, String)])
        text job

    get "/retrieve/:job/exists" $ do
        job <- param "job" :: ActionM String
        [Only result] <- liftIO (runFunction "select tasks_get_next(?)" (Only job) :: IO [Only (Maybe Int)])
        text $ T.pack $ show $ isJust result

    get "/retrieve/:job/id" $ do
        job <- param "job" :: ActionM String
        [Only result] <- liftIO (runFunction "select tasks_get_next(?)" (Only job) :: IO [Only (Maybe Int)])
        text $ T.pack $ show result

    get "/retrieve/:job/new-worker/:name" $ do
        job <- param "job" :: ActionM String
        name <- param "name" :: ActionM String

        [Only worker_id] <- liftIO (runFunction "select workers_insert(?, ?)" (name, job) :: IO [Only Int])
        text $ T.pack $ show worker_id

    post "/report/:job/:id/data" $ do
        -- inputData <- jsonData
        pure ()

    post "/report/:job/:id/file" $ do
        -- inputData <- jsonData
        pure ()

    post "/submit/new-job" $ do
        jobName <- param "jobName" :: ActionM String

        liftIO (runFunction "select jobs_insert(?)" (Only jobName) :: IO [Only ()])
        pure ()

