module Process where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import System.Environment (setEnv, unsetEnv)
import System.Exit
import System.Process

import Types

runScript :: String -> Map String String -> IO ExitCode
runScript scriptName vars = do
    mapM_ (uncurry setEnv) $ Map.toList vars

    (exitCode, _, _) <- readProcessWithExitCode scriptName [] ""

    mapM_ unsetEnv $ Map.keys vars

    pure exitCode

sysCommand :: SystemCommand -> IO String
sysCommand (SystemCommand str) = readCreateProcess (shell str) ""

sysCommandExitCode :: SystemCommand -> IO (ExitCode, String, String)
sysCommandExitCode (SystemCommand str) = readCreateProcessWithExitCode (shell str) ""

runSysCommands :: Map String String -> [SystemCommand] -> IO (ExitCode, String, String)
runSysCommands vars commands = do
    mapM_ (uncurry setEnv) $ Map.toList vars

    results <- go (ExitSuccess, "", "") commands

    mapM_ unsetEnv $ Map.keys vars

    pure results

    where
        go (curExitCode, curOur, curErr) [] = pure (curExitCode, curOur, curErr)
        go (curExitCode, curOur, curErr) (c:cs) = do
            (exitCode, stdout, stderr) <- sysCommandExitCode c

            case exitCode of
                ExitFailure _ -> pure (exitCode, curOur ++ stdout, curErr ++ stderr)
                ExitSuccess -> go (exitCode, curOur ++ stdout, curErr ++ stderr) cs

