{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad

import GHC.Conc.Sync

import Args
import Client.Client
import Server

main :: IO ()
main = handleArgs =<< getArgs

checkSafety :: Args -> (Int -> IO ()) -> IO ()
checkSafety args a = do
    procNum <- getNumProcessors

    let n = if args ^. maxProcs then procNum - 1 else args ^. number

    if not (args ^. noPrompt) then
        if n >= procNum then do
            putStrLn $ "You are about to start running " ++ show n ++ " tasks on a computer with " ++ show procNum ++ " processors. It is recommended that you run no more than one tasks per processor, and that you leave one processor free at all times. Continue (Y/N)?"

            response <- getLine

            when (response == "Y") $ a n
        else if n > 10 then do
            putStrLn $ "You are about to start running " ++ show n ++ " tasks. Are you sure you wish to continue (Y/N)?"

            response <- getLine

            when (response == "Y") $ a n
        else
            a n
    else
        a n

runWorkers :: Args -> IO ()
runWorkers args = checkSafety args $ \n -> do
    asyncs <- mapM (\_ -> async (runJob (args ^. job))) [1..n]

    -- cancel them all if any one of them throws anything (will handle keyboard interrupts too).
    onException (mapM_ wait asyncs) $ mapM_ cancel asyncs

handleArgs :: Args -> IO ()
handleArgs args =
    case args ^. command of
        "server" ->
            case args ^. job of
                "start" -> server
                str -> putStrLn $ "Unknown server command: " ++ str
        "submit" -> submitJob $ args ^. job
        "update" -> submitJobConfig (args ^. job) >> pure ()
        "run" -> runWorkers args
        "setup" -> setupJob (args ^. job) >> pure ()
        -- Create a new config.yaml from a default template.
        "new" -> pure ()

