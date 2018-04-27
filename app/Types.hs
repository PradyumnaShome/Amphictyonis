{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Yaml

import Network.URI

data SourceType = Git | Server
    deriving (Show, Eq)

data Source = Source
    { _sourceType :: SourceType
    , _sourceURI :: String }
    deriving Show
makeLenses ''Source

newtype SystemCommand = SystemCommand String
    deriving Show

data File = File
    { _name :: String
    , _source :: Maybe Source
    , _filePath :: FilePath
    , _isDirectory :: Bool
    , _script :: [SystemCommand]
    , _contents :: [File] }
    deriving Show
makeLenses ''File

data Config = Config
    { _jobName :: String
    , _jobVersion :: Maybe String
    , _workerRoot :: File }
    deriving Show

makeLenses ''Config

data FileOperation = Mkdir | Touch
    deriving Show

data FileSolution = GitClone String String
                  | ServerRequest String String
                  | FileOp FileOperation String
                  | RunCommand SystemCommand
    deriving Show

getSourceType :: String -> SourceType
getSourceType "git" = Git
getSourceType "server" = Server
getSourceType name = error $ "Error: Unknown source type: '" ++ name ++ "'"

parseSource :: String -> Source
parseSource strURI =
    case parseURI strURI of
        Nothing -> error $ "Invalid uri: " ++ strURI
        Just uri -> Source (getSourceType (init (uriScheme uri))) (uriToString id uri "")

directoryLike :: [File] -> Maybe Source -> Bool
directoryLike _ Nothing = True -- If we don't get the file from anywhere, let's assume it's a directory.
directoryLike contents (Just source) =
    source ^. sourceType == Git ||
    not (null contents) ||
    "/" `isSuffixOf` (source ^. sourceURI)

instance FromJSON SystemCommand where
    parseJSON x = SystemCommand <$> parseJSON x -- It's just a list of strings, so we don't have to write it ourselves.

instance FromJSON File where
    parseJSON (Object obj) = do
        strURI <- obj .:? "source"
        children <- obj .:? "dir"

        script <- obj .:? "script"

        let source = parseSource <$> strURI

        contents <- mapM parseJSON (fromMaybe [] children)

        File <$> obj.:"name"
             <*> pure source
             <*> obj.:"name"
             <*> pure (directoryLike contents source)
             <*> mapM parseJSON (fromMaybe [] script)
             <*> pure contents

instance FromJSON Config where
    parseJSON (Object obj) =
        Config <$> obj.:"name"
               <*> obj.:?"version"
               <*> (parseJSON =<< obj.:"worker-dir")

