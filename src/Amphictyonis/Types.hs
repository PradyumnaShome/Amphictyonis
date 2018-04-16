{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Amphictyonis.Types where

import Control.Applicative
import Control.Lens

import Data.List (nubBy)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup (Semigroup, (<>))
import Data.String

class Arg a where
    defaultArg :: a
    stringToArg :: String -> a -> a

instance Arg a => IsString (a -> a) where
    fromString = stringToArg

data Dependency = Dependency
    { _retrieveKind :: Maybe String
    , _sourceLocation :: Maybe String
    , _checkProgram :: Maybe String
    , _installProgram :: Maybe String
    , _location :: String
    , _dependencyName :: String }
    deriving Show
makeLenses ''Dependency

instance Arg Dependency where
    defaultArg = Dependency
        { _retrieveKind = Nothing
        , _sourceLocation = Nothing
        , _checkProgram = Nothing
        , _installProgram = Nothing
        , _location = "."
        , _dependencyName = "" }

    stringToArg = set dependencyName

data FileOp = FileOp
    { _path :: FilePath
    , _dest :: FilePath }
    deriving Show
makeLenses ''FileOp

instance Arg FileOp where
    defaultArg = FileOp
        { _path = "."
        , _dest = "." }

    stringToArg = set path

data InputSource = FromCommand Exec | InputFile String -- FromCommand gives us pipes.
    deriving Show

data OutputSource = Stdout | LogFile String
    deriving Show

instance IsString InputSource where
    fromString = InputFile

instance IsString OutputSource where
    fromString = LogFile

data Exec = Exec
    { _arguments :: [String]
    , _input :: Maybe InputSource
    , _output :: Maybe OutputSource
    , _command :: String }
    deriving Show
makeLenses ''Exec

instance Arg Exec where
    defaultArg = Exec
        { _arguments = []
        , _input = Nothing
        , _output = Nothing
        , _command = "" }

    stringToArg = set command

data CompOp = Equal | Not CompOp | FileExists | ExecutableExists
    deriving Show

data Expr a = Comp CompOp (Expr String) (Maybe (Expr String))
            | VarRef String
            | Literal String
            | Execute Exec
    deriving Show

data Statement = If (Expr Bool) [Statement] [Statement]
               | ExprSt (Expr String)
    deriving Show

data Script a = Script [Dependency] [Statement]
    deriving Show

instance Semigroup (Script a) where
    Script aDeps aSts <> Script bDeps bSts =
        Script (nubBy (\a b -> a ^. dependencyName == b ^. dependencyName) (aDeps ++ bDeps))
               (aSts ++ bSts)

instance Monoid (Script a) where
    mempty = Script [] []
    mappend = (<>)

executableExists :: String -> Expr Bool
executableExists name = Comp ExecutableExists (Literal name) Nothing

fileExists :: String -> Expr Bool
fileExists name = Comp FileExists (Literal name) Nothing

invert :: Expr Bool -> Expr Bool
invert (Comp compOp a b) = Comp (Not compOp) a b
invert expr = expr

ifStatement :: Expr Bool -> Script Int -> Script Int -> Script Int
ifStatement cond (Script thenDeps thenSts) (Script elseDeps elseSts)  =
    Script (thenDeps ++ elseDeps) [If cond thenSts elseSts]

with :: String -> Dependency -> Dependency
with = set installProgram . Just

from :: String -> Dependency -> Dependency
from str dep =
    case str of
        "git" ->
            if dep ^. location == "." then
                case splitOn "/" <$> dep ^. sourceLocation of
                    Just [] -> set retrieveKind (Just str) dep
                    Just locationParts -> set location (last locationParts) $ set retrieveKind (Just str) dep
                    Nothing -> set retrieveKind (Just str) dep
            else
                set retrieveKind (Just str) dep
        _ -> set retrieveKind (Just str) dep

url :: String -> Dependency -> Dependency
url str dep =
    case dep ^. retrieveKind of
        Just "git" -> set location (last (splitOn "/" str)) $ set sourceLocation (Just str) dep
        _ -> set sourceLocation (Just str) dep

checkInstalled :: (Dependency -> Dependency) -> Script Int -> Script Int
checkInstalled f installScript =
    let tempArgs = f defaultArg in
        if tempArgs ^. location == "."  then
            ifStatement (invert (executableExists (tempArgs ^. dependencyName))) installScript mempty
        else
            ifStatement (invert (fileExists (tempArgs ^. location))) installScript mempty

install :: (Dependency -> Dependency) -> Script Int
install f = let tempArgs = f defaultArg
                runArgs = case tempArgs ^. installProgram of
                            Just "stack" -> ["install"]
                            _ -> []
                script = cd (fromString (tempArgs ^. location)) <> run (fromString (fromMaybe "" (tempArgs ^. installProgram)) . args runArgs)
                in
                Script (map (($ defaultArg) . stringToArg) $ catMaybes [tempArgs ^. installProgram, tempArgs ^. retrieveKind ]) [] <>
                script

pipe :: (Exec -> Exec) -> Exec -> Exec
pipe = inputFrom . FromCommand . ($ defaultArg)

inputFrom :: InputSource -> Exec -> Exec
inputFrom = set input . Just

outputTo :: OutputSource -> Exec -> Exec
outputTo = set output . Just

cd :: (FileOp -> FileOp) -> Script Int
cd f = let tempArgs = f defaultArg in
           run ("cd" . args [tempArgs ^. path])

args :: [String] -> (Exec -> Exec)
args newArgs = over arguments (newArgs ++)

run :: (Exec -> Exec) -> Script Int
run f = Script [] [ExprSt $ Execute $ f defaultArg]

runScript :: (Exec -> Exec) -> Script Int
runScript f = run ("run" . f)

destination :: FilePath -> FileOp -> FileOp
destination = set dest

copy :: (FileOp -> FileOp) -> Script Int
copy f = let tempArgs = f defaultArg in
             run ("cp" . args [tempArgs ^. path, tempArgs ^. dest])

rm :: (FileOp -> FileOp) -> Script Int
rm f = let tempArgs = f defaultArg in
           run ("rm" . args [tempArgs ^. path])

var :: String -> Expr String
var = VarRef

makeScript :: [Script a] -> Script a
makeScript = mconcat

echo :: String -> Script Int
echo str = run ("echo" . args [str])

