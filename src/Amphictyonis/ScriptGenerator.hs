{-# LANGUAGE OverloadedStrings #-}

module Amphictyonis.ScriptGenerator where

import Control.Lens

import Data.List (intercalate)
import Data.Semigroup ((<>))

import Amphictyonis.Types

data OS = Windows | MacOS | Linux Dist
    deriving Eq

instance Show OS where
    show Windows = "Windows"
    show MacOS = "MacOS"
    show (Linux dist) = show dist

data Dist = Ubuntu | Other
    deriving (Eq, Show)

getOsSpecific :: OS -> String -> String
getOsSpecific Windows str =
    case str of
        "run" -> "Invoke-Command -FilePath"
        _ -> str
getOsSpecific _ str =
    case str of
        "run" -> "bash"
        _ -> str

generate :: OS -> Script Int -> String
generate os (Script deps sts) =
    intercalate "\n" (map (generateDep os) deps) ++
    intercalate "\n" (concatMap (generateSt os) sts) ++ "\n\n"

tryPackageManagers :: [String] -> String -> Script Int
tryPackageManagers mgrs dependencyName =
    foldl (\cur mgr -> ifStatement (executableExists mgr) (installWith mgr) cur)
          (echo ("No known package managers installed from this list: " ++ intercalate "," mgrs))
          mgrs
    where
        installWith mgr = run ("sudo" . args [mgr, "install", dependencyName])

defaultInstall :: OS -> String -> Script Int
defaultInstall Windows dependencyName = run ("Install-Package" . args ["-Name", dependencyName])
defaultInstall _ "stack" =
    run ("sh" . pipe ("wget" . args ["-qO-", "https://get.haskellstack.org/"]))
defaultInstall (Linux Ubuntu) dependencyName = tryPackageManagers ["apt-get", "apt", "aptitude"] dependencyName
defaultInstall MacOS dependencyName = tryPackageManagers ["brew", "port"] dependencyName
-- Try everything.
defaultInstall _ dependencyName = tryPackageManagers ["apt-get", "apt", "aptitude", "yum", "dnf"] dependencyName

generateDep :: OS -> Dependency -> String
generateDep os dep = generate os $ checkInstalled (const dep) $ defaultInstall os $ dep ^. dependencyName

generateExpr :: OS -> Expr a -> String
generateExpr os (Comp compOp a b) = generateCond os $ Comp compOp a b
generateExpr os (Literal str) = show str
generateExpr os (VarRef varName) =
    case os of
        Windows -> "$" ++ varName
        _ -> "${" ++ varName ++ "}"
generateExpr os (Execute exec) = generateInput os (exec ^. input) $ generateOutput os (exec ^. output) execStr
    where
        execStr = getOsSpecific os (exec ^. command) ++ " " ++ unwords args
        args = exec ^. arguments --map (generateExpr os) $ exec ^. arguments

generateInput :: OS -> Maybe InputSource -> String -> String
generateInput os Nothing str = str
generateInput os (Just (FromCommand exec)) str = generateExpr os (Execute exec) ++ " | " ++ str
generateInput os (Just (InputFile path)) str =
    case os of
        Windows -> "Get-Content " ++ path ++ " | " ++ str
        _ -> "cat " ++ path ++ " | " ++ str

generateOutput :: OS -> Maybe OutputSource -> String -> String
generateOutput os Nothing str = str
generateOutput os (Just Stdout) str = str
generateOutput os (Just (LogFile path)) str = str ++ " | tee " ++ path

generateCond :: OS -> Expr Bool -> String
generateCond Windows (Comp compOp a b) = ""
generateCond os expr@(Comp compOp a b) =
    case compOp of
        Not newCompOp -> "! " ++ generateCond os (Comp newCompOp a b)
        Equal ->
            case b of
                Nothing -> error $ "Trying to compare using Equal but no second operand was provided: '" ++ show expr ++ "'"
                Just bExpr -> "[ " ++ generateExpr os a ++ " = " ++ generateExpr os bExpr ++ " ]"
        FileExists -> "[ -e " ++ generateExpr os a ++ " ]"
        ExecutableExists -> "command -v " ++ generateExpr os a

generateCond os expr = error $ "Error: Unsupported condition expression '" ++ show expr ++ "' for OS '" ++ show os ++ "'"

generateIf :: OS -> String -> [String] -> [String] -> [String]
generateIf Windows cond thenStr elseStr =
    ["If (" ++ cond ++ ") {"] ++ thenStr ++ ["}"] ++
    if not $ null elseStr then
        [" Else {"] ++ elseStr ++ ["}"]
    else
        []
generateIf os cond thenStr elseStr =
    ["if " ++ cond ++ "; then"] ++ thenStr ++
    if not $ null elseStr then
        ["else"] ++ elseStr ++ ["fi"]
    else
        ["fi"]

generateSt :: OS -> Statement -> [String]
generateSt os (ExprSt expr) = [generateExpr os expr]
generateSt os (If cond thenBlock elseBlock) = generateIf os condStr thenStr elseStr
    where
        condStr = generateCond os cond
        thenStr = map ("    " ++) $ concatMap (generateSt os) thenBlock
        elseStr = map ("    " ++) $ concatMap (generateSt os) elseBlock

