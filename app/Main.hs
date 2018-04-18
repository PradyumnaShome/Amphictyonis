{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens

import Args
import Client
import Server

main :: IO ()
main = handleArgs =<< getArgs

handleArgs :: Args -> IO ()
handleArgs args = do
    print args
    case args ^. command of
        "server" ->
            case args ^. job of
                "start" -> server
                str -> putStrLn $ "Unknown server command: " ++ str
        "new-job" -> createNewJob $ args ^. job
        "retrieve" -> do
            pure ()
        "report" -> do
            pure ()
        "runner" ->
            case args ^. file of
                Nothing -> putStrLn "Must provide a script to run with -f!"
                Just fname -> runner (args ^. job) fname

