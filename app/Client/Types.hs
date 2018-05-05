{-# LANGUAGE TemplateHaskell #-}

module Client.Types where

import Control.Lens

import Args
import Types

data WorkerState = WorkerState
    { _workerId :: Int
    , _workerPath :: FilePath
    , _args :: Args
    , _config :: Config }
    deriving Show
makeLenses ''WorkerState

