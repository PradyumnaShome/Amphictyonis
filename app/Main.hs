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
        "init" -> initJob $ args ^. job
        "run" -> runJob (args ^. job)
        -- Create a new config.yaml from a default template.
        "new" -> pure ()

