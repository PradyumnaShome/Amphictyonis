{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens

import Data.Map (Map)
import qualified Data.Map as Map

import System.Environment (setEnv, unsetEnv)
import System.Exit
import System.Process

import Args
import Client
import Server

main :: IO ()
main = handleArgs =<< getArgs

runScript :: String -> Map String String -> IO ExitCode
runScript scriptName vars = do
    mapM_ (uncurry setEnv) $ Map.toList vars

    (exitCode, _, _) <- readProcessWithExitCode scriptName [] ""

    mapM_ unsetEnv $ Map.keys vars

    pure exitCode

handleArgs :: Args -> IO ()
handleArgs args = do
    print args
    case args ^. command of
        "server" ->
            case args ^. job of
                "start" -> server
                str -> putStrLn $ "Unknown server command: " ++ str
        "init" -> initJob $ args ^. job
        "runner" ->
            case args ^. file of
                Nothing -> putStrLn "Must provide a script to run with -f!"
                Just fname -> runner (args ^. job) $ runScript fname

