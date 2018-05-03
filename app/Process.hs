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

sysCommandExitCode :: FilePath -> SystemCommand -> IO (ExitCode, String, String)
sysCommandExitCode pwd (SystemCommand str) = readCreateProcessWithExitCode ((shell str) {cwd = Just pwd}) ""

runSysCommands :: FilePath -> Map String String -> [SystemCommand] -> IO (ExitCode, String, String)
runSysCommands pwd vars commands = do
    mapM_ (uncurry setEnv) $ Map.toList vars

    results <- go (ExitSuccess, "", "") commands

    mapM_ unsetEnv $ Map.keys vars

    pure results

    where
        go (curExitCode, curOur, curErr) [] = pure (curExitCode, curOur, curErr)
        go (curExitCode, curOur, curErr) (c:cs) = do
            (exitCode, stdout, stderr) <- sysCommandExitCode pwd c

            case exitCode of
                ExitFailure _ -> pure (exitCode, curOur ++ stdout, curErr ++ stderr)
                ExitSuccess -> go (exitCode, curOur ++ stdout, curErr ++ stderr) cs

