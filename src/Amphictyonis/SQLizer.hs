{-
    Data set type is inferred, and is one of {CSV, JSON, XML}.
    Data is parsed to Haskell data structures.
    A prepared statement is created based on the headings.
    A connection to postgres is established, and it is upserted into the table.
-}
module Amphictyonis.TaskResultManager.SQLizer where

import Xeno
import Data.Csv
import Data.List.Split
import Data.Text
import Data.Label
import System.IO

data TaskResult = TaskResult {
    format :: String,
    rawData :: String,
} deriving (Show)

-- Takes an XML filepath and returns a Xeno.DOM object
parseXml :: String -> Xeno.DOM
parseXml filepath = 
    
readTaskResultFromFile :: IO String -> TaskResult
readTaskResultFromFile filePath = do
    -- Figure out the file extension 
    extension = toLower ((endBy "." filePath) !! 1)
    -- Read file
    file <- openFile filePath ReadMode

    contents <- hGetContents file
    -- Close the file
    hClose file
    putStr contents
    -- Instantiate the result
    TaskResult result = TaskResult {
        format = extension,
        rawData = contents
    }
    parse result

parse :: TaskResult -> IO
parse result =
    let output = case result (get format result) of
        "xml" -> parseXml result
        "json" -> parseJson result
        "csv" -> parseCsv result