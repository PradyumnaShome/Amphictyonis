{-# LANGUAGE OverloadedStrings #-}

module Amphictyonis.Script
    (
        module Amphictyonis.Script
    ) where

import Data.Semigroup ((<>))

import System.Directory (createDirectoryIfMissing)

import Amphictyonis.Types
import Amphictyonis.ScriptGenerator

oses :: [OS]
oses = [Windows, MacOS, Linux Ubuntu, Linux Other]

getExtension :: OS -> String
getExtension Windows = "ps1"
getExtension _ = "sh"

makeRunScript :: OS -> String
makeRunScript os = generate os $ makeScript $
               -- On windows we need to enable the execution of powershell scripts.
               [run ("Set-ExecutionPolicy" . args ["Unrestricted"]) | os == Windows] ++
               [
                   runScript (args ["setup." ++ getExtension os]),
                   runScript (args ["runner." ++ getExtension os]),
                   runScript (args ["cleanup." ++ getExtension os])
               ]

createScripts :: OS -> Script Int -> Script Int -> Script Int -> (String, String, String, String)
createScripts os setup runner cleanup =
    (generate os setup, generate os runner, generate os cleanup, makeRunScript os)

writeScripts :: Script Int -> Script Int -> Script Int -> FilePath -> IO ()
writeScripts setup runner cleanup outputDir = mapM_ (\os -> go os (createScripts os setup runner cleanup)) oses
    where
        go os (setupStr, runnerStr, cleanupStr, runStr) = do
            createDirectoryIfMissing True $ outputDir ++ "/" ++ show os

            putStrLn $ "Writing " ++ (outputDir ++ "/" ++ show os ++ "/" ++ "setup." ++ getExtension os)
            writeFile (outputDir ++ "/" ++ show os ++ "/" ++ "setup." ++ getExtension os) setupStr
            putStrLn $ "Writing " ++ (outputDir ++ "/" ++ show os ++ "/" ++ "runner." ++ getExtension os)
            writeFile (outputDir ++ "/" ++ show os ++ "/" ++ "runner." ++ getExtension os) runnerStr
            putStrLn $ "Writing " ++ (outputDir ++ "/" ++ show os ++ "/" ++ "cleanup." ++ getExtension os)
            writeFile (outputDir ++ "/" ++ show os ++ "/" ++ "cleanup." ++ getExtension os) cleanupStr

            putStrLn $ "Writing " ++ (outputDir ++ "/" ++ show os ++ "/" ++ "run." ++ getExtension os)
            writeFile (outputDir ++ "/" ++ show os ++ "/" ++ "run." ++ getExtension os) runStr

