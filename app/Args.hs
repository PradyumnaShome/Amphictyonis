{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Args where

import Control.Lens (makeLenses, (^.))

import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Yaml

import Options.Applicative as Opt

import System.Directory
import System.FilePath

data Args = Args
    { _command :: String,
      _job :: String,
      _number :: Int,
      _noPrompt :: Bool,
      _maxProcs :: Bool,
      _host :: String,
      _port :: Int }
    deriving Show
makeLenses ''Args

-- For loading from amphi.yaml in ~/amphi.yaml
instance FromJSON Args where
    parseJSON (Object obj) = do
        n <- obj .:? "n"
        number <- obj .:? "number"

        Args ""
            <$> (fromMaybe "" <$> obj .:? "job")
            <*> pure (read (fromMaybe "1" (n <|> number)))
            <*> (fromMaybe False <$> (obj .:? "no-prompt"))
            <*> (fromMaybe False <$> (obj .:? "max"))
            <*> (fromMaybe "reedoei.com" <$> (obj .:? "host"))
            <*> (fromMaybe 3000 <$> (obj .:? "port"))

baseArgs = Args "" "" 1 False False "reedoei.com" 3000

knownCommands = ["server", "submit", "update", "run", "setup", "new", "list"]

argParser :: Opt.Parser Args
argParser = Args
    <$> argument str (completer (listCompleter knownCommands) <> help "Command to run.")
    <*> (fromMaybe "" <$> optional (argument str (help "The job to run.")))
    <*> (fromMaybe 1 <$> optional (option auto (long "number" <> short 'n' <> help "How many workers to run.")))
    <*> flag False True (long "no-prompt" <> help "Whether to prompt the user for sanity checking. Generally, shouldn't be used.")
    <*> flag False True (long "max" <> help "Run the job on as n - 1 processors (that is, leaving one processor free).")
    <*> (fromMaybe "reedoei.com" <$> optional (option str (long "host" <> short 'h' <> help "The Amphictyonis server host.")))
    <*> (fromMaybe 3000 <$> optional (option auto (long "port" <> short 'p' <> help "The port the Amphictyonis server is hosted on.")))

-- | Returns whichever list is non-empty, preferring the first. If both are empty, returns the first.
nonEmpty field a b
    | null (a ^. field) && not (null (b ^. field)) = b ^. field
    | otherwise = a ^. field

-- | Returns the first value if it is not the default, otherwise returns the second value.
nonDefault field def a b
    | (a ^. field) /= def = a ^. field
    | otherwise = b ^. field

mergeArgs :: Args -> Args -> Args
mergeArgs a b = Args

    { _command = nonEmpty Args.command a b
    , _job = nonEmpty job a b
    , _number = nonDefault number 1 a b
    , _noPrompt = nonDefault noPrompt False a b
    , _maxProcs = nonDefault maxProcs False a b
    , _host = nonDefault host "reedoei.com" a b
    , _port = nonDefault port 3000 a b }

getArgs = do
    clArgs <- execParser opts

    home <- getHomeDirectory
    defaultArgs <- decodeFileEither $ joinPath [home, "amphi.yaml"]

    case defaultArgs of
        Left _ -> pure clArgs
        Right args -> pure $ mergeArgs clArgs args

    where
        opts = info (argParser <**> helper)
                (fullDesc <>
                 progDesc "Amphictyonis is a system for parallelizing programs and scripts." <>
                 header "amphi")

