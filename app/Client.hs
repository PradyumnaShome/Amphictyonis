{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import Control.Lens
import Control.Monad
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State

import Data.Aeson
import Data.Aeson.Lens (key, nth, _Object)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromMaybe)

import Database.PostgreSQL.Simple

import Network.HostName
import Network.URI
import Network.Wreq

import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Database
import Types

whenM :: Monad m => m Bool -> m () -> m ()
whenM v a = v >>= (`when` a)

requestFile :: String -> FilePath -> IO B.ByteString
requestFile jobName fname = do
    response <- get $ "http://localhost:3000/retrieve/" ++ jobName ++ "/request-file/" ++ fname
    pure $ response ^. responseBody

gitClone :: String -> FilePath -> IO ()
gitClone gitUrl destination = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode "git" ["clone", gitUrl, destination] ""
    putStrLn stdout
    putStrLn stderr
    putStrLn $ "Exited with " ++ show exitCode

getPath :: FileSolution -> FilePath
getPath (GitClone repoUrl destPath) = destPath
getPath (ServerRequest requestUrl destPath) = destPath
getPath (FileOp op path) = path
getPath _ = ""

addPath :: FilePath -> FileSolution -> FileSolution
addPath base (GitClone repoUrl destPath) = GitClone repoUrl (joinPath [base, destPath])
addPath base (ServerRequest requestUrl destPath) = ServerRequest requestUrl (joinPath [base, destPath])
addPath base (FileOp op path) = FileOp op (joinPath [base, path])
addPath _ i = i

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

solveFiles :: FilePath -> File -> StateT Config IO [FileSolution]
solveFiles rootPath file =
    let sol = case file ^. source of
                Just source ->
                    case source ^. sourceType of
                        Git -> GitClone (source ^. sourceURI) (file ^. name)
                        Server -> ServerRequest (source ^. sourceURI) (file ^. name)
                Nothing -> if file ^. isDirectory then
                                FileOp Mkdir $ file ^. name
                           else
                                FileOp Touch $ file ^. name
        scriptSolution = map RunCommand (file ^. script)
        in do
    pathedSol <- State.lift $ useUnusedPath $ addPath rootPath sol
    State.modify $ over workerRoot (setPath (file ^. name) (getPath pathedSol))

    contentsSolution <- concat <$> mapM (solveFiles (getPath pathedSol)) (file ^. contents)

    pure $ pathedSol : scriptSolution ++ contentsSolution

applyFileSolution :: String -> FileSolution -> IO ()
applyFileSolution jobName sol = do
    putStrLn $ "Applying operation: " ++ show sol

    case sol of
        GitClone gitUrl destination -> gitClone gitUrl destination
        ServerRequest serverUrl destination ->
            case parseURI serverUrl of
                Just uri -> do
                    fileContents <- requestFile jobName $ uriPath uri
                    B.writeFile destination fileContents
                Nothing -> error $ "Could not parse URI: '" ++ serverUrl ++ "'"

        FileOp Mkdir dirName -> createDirectory dirName
        FileOp Touch fname -> whenM (not <$> doesFileExist fname) $ writeFile fname ""
        RunCommand (SystemCommand str) -> system str >> pure ()

initStructure :: Config -> IO ()
initStructure config = do
    -- let jobName = config ^. jobName
    -- let versionName = config ^. version

    putStrLn "Trying to solve file dependencies..."
    (solution, updatedConfig) <- State.runStateT (solveFiles "." (config ^. workerRoot)) config

    putStrLn "Solution:"
    mapM_ print solution

    putStrLn "Applying solution."

    -- mapM_ (applyFileSolution (config ^. jobName)) solution

newWorker :: String -> IO Int
newWorker job = do
    hostName <- getHostName

    let url = "http://localhost:3000/retrieve/" ++ job ++ "/new-worker/" ++ hostName
    response <- get url

    case read . Char8.unpack <$> response ^? responseBody of
        -- TODO: Better error handling.
        Nothing -> error $ show response
        Just i -> pure i

isWork :: String -> IO Bool
isWork jobName = do
    let url = "http://localhost:3000/retrieve/" ++ jobName ++ "/exists"
    response <- get url

    pure $ fromMaybe False $ read . Char8.unpack <$> response ^? responseBody

getWork :: String -> Int -> IO (Int, Map String String)
getWork jobName workerId = do
    let url = "http://localhost:3000/retrieve/" ++ jobName ++ "/data"
    response <- asValue =<< get url

    let obj = response ^. responseBody . key "json" . _Object

    case (,) <$> (fromJSON <$> HashMap.lookup "taskId" obj)
             <*> (fromJSON <$> HashMap.lookup "data" obj) of
        Just (Success taskId, Success d) -> pure (taskId, d)
        _ -> error "Failed" -- TODO: Better error handling.

reportStatus :: Int -> Int -> String -> IO ()
reportStatus workerId taskId status = do
    let url = "http://localhost:3000/report/" ++ show taskId ++ "/" ++ show workerId ++ "/status"
    post url $ partString "status" status

    -- TODO: Handle an error.
    pure ()

runner :: String -> (Map String String -> IO System.Exit.ExitCode) -> IO ()
runner jobName doWork = do
    workerId <- newWorker jobName
    putStrLn $ "Got worker id: " ++ show workerId

    let workingPath = "worker-" ++ show workerId
    whenM (doesDirectoryExist workingPath) $ removeDirectoryRecursive workingPath
    createDirectory workingPath
    putStrLn $ "Cleared out and created directory: " ++ workingPath

    putStrLn "Beginning runner loop."
    runnerLoop workerId

    where
        runnerLoop workerId =
            whenM (isWork jobName) $ do
                (taskId, jobData) <- getWork jobName workerId
                reportStatus taskId workerId "running"
                exitCode <- doWork jobData

                case exitCode of
                    ExitSuccess -> do
                        reportStatus taskId workerId "finished"
                        runnerLoop workerId
                    ExitFailure _ -> do
                        -- TODO: Look at the exit code.
                        reportStatus taskId workerId "failed"
                        runnerLoop workerId

-----------------------------------------------
-- These functions can only be run on the server where the PostgreSQL server is hosted.
----------------------------------------------
initJob :: FilePath -> IO ()
initJob configPath =
    -- runFunction "select jobs_insert(?)" (Only jobName) :: IO [Only ()]
    pure ()

-- | Finds changed and modified files. Used to automatically gather results without needing to specify them.
-- discoverFiles :: FilePath -> IO [FilePath]

-- while amphi retrieve --job sample --exists
-- do
--     id=$(amphi retrieve sample --next-id)
--     args=$(amphi retrieve --job sample --id "$id" --data)

--     report="amphi report --job sample --id \"$id\" --worker \"$worker_id\""

--     $report -s "running"
--     bash runner.sh $args

--     if [[ "$?" -eq "0" ]]; then
--         $report -s "finished"
--     else
--         $report -s "failed"
--     fi
-- done

