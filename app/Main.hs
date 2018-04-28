{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens

import Args
import Client.Client
import Process
import Server

main :: IO ()
main = handleArgs =<< getArgs

handleArgs :: Args -> IO ()
handleArgs args =
    case args ^. command of
        "server" ->
            case args ^. job of
                "start" -> server
                str -> putStrLn $ "Unknown server command: " ++ str
        "submit" -> submitJob $ args ^. job
        "update" -> submitJobConfig (args ^. job) >> pure ()
        "run" -> runJob (args ^. job)
        "setup" -> setupJob (args ^. job) >> pure ()
        -- Create a new config.yaml from a default template.
        "new" -> pure ()

