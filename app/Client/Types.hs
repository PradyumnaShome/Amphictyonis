{-# LANGUAGE TemplateHaskell #-}

module Client.Types where

import Control.Lens

import Types

data WorkerState = WorkerState
    { _workerId :: Int
    , _workerPath :: FilePath
    , _config :: Config }
    deriving Show
makeLenses ''WorkerState

