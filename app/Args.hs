{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Args where

import Control.Lens (makeLenses)

import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))

import Options.Applicative

data Args = Args
    { _command :: String,
      _job :: String,
      _number :: Int,
      _noPrompt :: Bool,
      _maxProcs :: Bool,
      _file :: Maybe FilePath }
    deriving Show
makeLenses ''Args

baseArgs = Args "" "" 1 False False Nothing

argParser :: Parser Args
argParser = Args
    <$> argument str (help "Command to run.")
    <*> argument str (help "The job to run.")
    <*> (fromMaybe 1 <$> optional (option auto (long "number" <> short 'n' <> help "How many workers to run.")))
    <*> flag False True (long "no-prompt" <> help "Whether to prompt the user for sanity checking. Generally, shouldn't be used.")
    <*> flag False True (long "max" <> help "Run the job on as n - 1 processors (that is, leaving one processor free).")
    <*> optional (option str (long "file" <> short 'f' <> help "The path to the file or directory relevant to the command"))

getArgs = execParser opts
    where
        opts = info (argParser <**> helper)
                (fullDesc <>
                 progDesc "Amphictyonis" <>
                 header "amphi")

