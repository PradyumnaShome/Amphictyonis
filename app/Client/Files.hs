{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.Files where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State

import qualified Codec.Compression.GZip as GZip

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.CSV
import Data.List (isPrefixOf, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock
import Data.Yaml

import Network.URI
import Network.Wreq

import System.Directory
import System.FilePath
import System.Process

import Text.ParserCombinators.Parsec

import Client.Types
import Process
import Types
import Util

requestFile :: String -> FilePath -> IO B.ByteString
requestFile jobName fname = do
    response <- get $ "http://localhost:3000/retrieve/" ++ jobName ++ "/request-file/" ++ fname
    pure $ GZip.decompress $ B64.decodeLenient $ response ^. responseBody

requestConfig :: String -> IO String
requestConfig jobName = do
    response <- get $ "http://localhost:3000/retrieve/" ++ jobName ++ "/config"
    pure $ Char8.unpack $ response ^. responseBody

uploadFile :: String -> Int -> FilePath -> IO ()
uploadFile jobName taskId fname = do
    let url = "http://localhost:3000/submit/" ++ jobName ++ "/" ++ show taskId ++ "/file"
    contents <- readFile fname
    let uploadPart = partLBS "file" $ B64.encode $ GZip.compress $ Char8.pack contents
    post url [uploadPart, partString "filename" fname]

    pure ()

readCSV :: String -> [[(String, String)]]
readCSV str =
    case parse csvFile "" str of
        Left err -> error $ "Error while parsing csv file: " ++ show err
        Right contents ->
            let header = head contents in
                map (zip header) $ tail contents

gitClone :: String -> FilePath -> IO ()
gitClone gitUrl destination =
    whenM (not <$> doesDirectoryExist destination) $ do
        (exitCode, stdout, stderr) <- readProcessWithExitCode "git" ["clone", gitUrl, destination] ""
        putStrLn stdout
        putStrLn stderr
        putStrLn $ "Exited with " ++ show exitCode

getPathByName :: Config -> String -> FilePath
getPathByName _ "." = "."
getPathByName config fileName = getPathByName' $ config ^. workerRoot
    where
        getPathByName' f
            | f ^. name == fileName = f ^. filePath
            | otherwise = head $ map getPathByName' $ f ^. contents

getResultPaths :: Config -> [FilePath]
getResultPaths config = map (getPathByName config) $ config ^. resultPaths

getWorkingPath :: Config -> FilePath
getWorkingPath config = getPathByName config $ config ^. workingPath

getPath :: FileSolution -> FilePath
getPath (GitClone repoUrl destPath) = destPath
getPath (ServerRequest requestUrl destPath) = destPath
getPath (FileOp op path) = path
getPath (RunCommand path _) = path

addPath :: FilePath -> FileSolution -> FileSolution
addPath base (GitClone repoUrl destPath) = GitClone repoUrl (joinPath [base, destPath])
addPath base (ServerRequest requestUrl destPath) = ServerRequest requestUrl (joinPath [base, destPath])
addPath base (FileOp op path) = FileOp op (joinPath [base, path])
addPath base (RunCommand path command) = RunCommand (joinPath [base, path]) command

getUnusedPath :: FilePath -> Int -> IO FilePath
getUnusedPath path i = do
    exists <- doesPathExist curPath

    if exists then
        getUnusedPath path (i + 1)
    else
        pure curPath
    where
        curPath
            | i == 0 = path
            | otherwise = path ++ show i

useUnusedPath :: FileSolution -> IO FileSolution
useUnusedPath (GitClone repoUrl destPath) = GitClone repoUrl <$> getUnusedPath destPath 0
useUnusedPath (ServerRequest requestUrl destPath) = ServerRequest requestUrl <$> getUnusedPath destPath 0
useUnusedPath (FileOp op path) = FileOp op <$> getUnusedPath path 0
useUnusedPath i = pure i

setPath :: String -> FilePath -> File -> File
setPath checkName newPath file
    | file ^. name == checkName = set filePath newPath file
    | otherwise = over contents (map (setPath checkName newPath)) file

solveFiles :: (MonadIO m, MonadState WorkerState m) => FilePath -> File -> m [FileSolution]
solveFiles parentPath file =
    let sol = case file ^. source of
                Just source ->
                    case source ^. sourceType of
                        Git -> GitClone (source ^. sourceURI) (file ^. name)
                        Server -> ServerRequest (source ^. sourceURI) (file ^. name)
                Nothing -> if file ^. isDirectory then
                                FileOp Mkdir $ file ^. name
                           else
                                FileOp Touch $ file ^. name
        in do
    reuse <- view (config . reusePaths) <$> State.get

    pathedSol <-
        if reuse then
            pure (addPath parentPath sol)
        else
            liftIO $ useUnusedPath (addPath parentPath sol)

    State.modify $ over (config . workerRoot) (setPath (file ^. name) (getPath pathedSol))

    contentsSolution <- concat <$> mapM (solveFiles (getPath pathedSol)) (file ^. contents)

    let scriptSolution
            | file ^. isDirectory = map (RunCommand (getPath pathedSol)) (file ^. script)
            | otherwise = map (RunCommand (takeDirectory (getPath pathedSol))) (file ^. script)

    pure $ pathedSol : scriptSolution ++ contentsSolution

applyFileSolution :: (MonadIO m, MonadState WorkerState m) => String -> FileSolution -> m ()
applyFileSolution jobName sol = do
    liftIO $ putStrLn $ "Applying operation: " ++ show sol

    liftIO $ case sol of
        GitClone gitUrl destination -> gitClone gitUrl destination
        ServerRequest serverUrl destination ->
            case parseURI serverUrl of
                Just uri -> do
                    fileContents <- requestFile jobName $ uriPath uri
                    B.writeFile destination fileContents
                Nothing -> error $ "Could not parse URI: '" ++ serverUrl ++ "'"

        FileOp Mkdir dirName -> createDirectoryIfMissing True dirName
        FileOp Touch fname -> whenM (not <$> doesFileExist fname) $ writeFile fname ""
        RunCommand dir com -> sysCommandExitCode dir com >> pure ()

-- | Creates the directory structure for the work, then returns the updated config.
initStructure :: (MonadIO m, MonadState WorkerState m) => m Config
initStructure = do
    liftIO $ putStrLn "Trying to solve file dependencies..."

    baseConfig <- view config <$> State.get
    rootPath <- view workerPath <$> State.get
    rootFile <- view (config . workerRoot) <$> State.get

    solution <- solveFiles rootPath rootFile

    liftIO $ putStrLn "Solution:"
    liftIO $ mapM_ print solution

    liftIO $ putStrLn "Applying solution."

    job <- view (config.jobName) <$> State.get
    mapM_ (applyFileSolution job) solution

    updatedConfig <- view config <$> State.get

    -- Reset config.
    State.modify $ set config baseConfig
    pure updatedConfig

readConfig :: FilePath -> IO Config
readConfig path = do
    f <- decodeFileEither path
    case f of
        Left err -> error $ show err
        Right config -> pure config

listFiles :: Bool -> FilePath -> IO (Map FilePath UTCTime)
listFiles visitHidden path = do
    allFiles <- listDirectory path

    let toCheck =
            if visitHidden then
                allFiles
            else
                filter (not . ("." `isPrefixOf`)) allFiles

    dirs <- filterM doesDirectoryExist $ map (\p -> joinPath [path,p]) toCheck

    rest <- foldl Map.union Map.empty <$> mapM (listFiles visitHidden) dirs

    let fs = map (\p -> joinPath [path, p]) allFiles
    fileModTimes <- mapM (\f -> (f,) <$> getModificationTime f) $ fs \\ dirs

    pure $ Map.union rest $ Map.fromList fileModTimes

