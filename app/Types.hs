{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Lens

import Data.Char
import Data.List (isSuffixOf, dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Yaml

import Network.URI

import System.FilePath

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
    , _workerRoot :: File
    , _reusePaths :: Bool
    , _workingPath :: String
    , _resultPaths :: [String]
    , _diffUpload :: Bool
    , _dataSources :: [SystemCommand]
    , _runner :: [SystemCommand] }
    deriving Show

makeLenses ''Config

data FileOperation = Mkdir | Touch
    deriving Show

data FileSolution = GitClone String String
                  | ServerRequest String String
                  | FileOp FileOperation String
                  | RunCommand FilePath SystemCommand
    deriving Show

trim = dropWhileEnd isSpace . dropWhile isSpace

getSourceType :: String -> SourceType
getSourceType "git" = Git
getSourceType "server" = Server
getSourceType name = error $ "Error: Unknown source type: '" ++ name ++ "'"

parseSource :: String -> Source
parseSource strURI =
    case parseURI strURI of
        Nothing -> error $ "Invalid uri: " ++ strURI
        Just uri -> Source (getSourceType (init (uriScheme uri))) (uriToString id uri "")

directoryLike :: String -> [File] -> Maybe Source -> Bool
-- If it doesn't have an extension, it's probably a directory.
directoryLike name _ Nothing = null (takeExtension name)
directoryLike name contents (Just source) =
    (source ^. sourceType == Git ||
     not (null contents) ||
     "/" `isSuffixOf` (source ^. sourceURI)) && null (takeExtension name)

instance FromJSON SystemCommand where
    parseJSON x = SystemCommand <$> parseJSON x -- It's just a list of strings, so we don't have to write it ourselves.

instance FromJSON File where
    parseJSON (Object obj) = do
        name <- obj .: "name"
        strURI <- obj .:? "source"
        children <- obj .:? "dir"

        script <- obj .:? "script"

        let source = parseSource <$> strURI

        contents <- mapM parseJSON (fromMaybe [] children)

        File <$> (trim <$> (obj.:"name"))
             <*> pure source
             <*> (trim <$> (obj.:"name"))
             <*> pure (directoryLike name contents source)
             <*> mapM parseJSON (fromMaybe [] script)
             <*> pure contents

instance FromJSON Config where
    parseJSON (Object obj) =
        Config <$> obj.:"name"
               <*> obj.:?"version"
               <*> (parseJSON =<< obj.:"worker-dir")
               <*> (fromMaybe True <$> obj.:?"reuse-paths")
               <*> (trim . fromMaybe "." <$> obj.:?"working-path")
               <*> (map trim <$> (mapM parseJSON =<< (fromMaybe ["."] <$> obj.:?"result-paths")))
               <*> (fromMaybe True <$> obj.:?"diff-upload")
               <*> (mapM parseJSON =<< (fromMaybe [] <$> obj .:? "data-sources"))
               <*> (mapM parseJSON =<< (fromMaybe [] <$> obj .:? "runner"))

