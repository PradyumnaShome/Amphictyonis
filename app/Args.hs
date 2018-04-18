{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Args where

import Control.Lens (makeLenses)

import Data.Semigroup ((<>))

import Options.Applicative

data Args = Args
    { _command :: String,
      _job :: String,
      _action :: Maybe String,
      _id :: Maybe String,
      _workerId :: Maybe String,
      _status :: Maybe String,
      _file :: Maybe FilePath }
    deriving Show
makeLenses ''Args

actions = ["exists", "new-worker", "data", "next-id"]

actionParser :: Parser (Maybe String)
actionParser = optional (option str (long "action" <> short 'a' <> help "The action to perform.")) <|>
               foldl (<|>) (pure Nothing) (map makeAction actions)
    where
        makeAction name = flag Nothing (Just name) (long name)

argParser :: Parser Args
argParser = Args
    <$> argument str (help "Command to run.")
    <*> argument str (help "The job to run.")
    <*> actionParser
    <*> optional (option str (long "id" <> short 'i' <> help "The task id that the action should be performed on."))
    <*> optional (option str (long "worker" <> short 'w' <> help "The worker id that is reporting."))
    <*> optional (option str (long "status" <> short 's' <> help "The status of some task to send to the server"))
    <*> optional (option str (long "file" <> short 'f' <> help "The path to the file or directory relevant to the command"))

getArgs = execParser opts
    where
        opts = info (argParser <**> helper)
                (fullDesc <>
                 progDesc "Amphictyonis" <>
                 header "amphi")

