{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.PostgreSQL.Simple

import Data.List (intercalate)
import Data.Int (Int64)

connection :: IO Connection
connection = connectPostgreSQL "host='localhost' user='amphictyonis' password='password'"

runFunction_ :: (FromRow r) => Query -> IO [r]
runFunction_ func = do
    conn <- connection
    result <- query_ conn func
    close conn

    pure result

runFunction :: (ToRow q, FromRow r) => Query -> q -> IO [r]
runFunction func args = do
    conn <- connection
    result <- query conn func args
    close conn

    pure result

executeFunction :: ToRow q => Query -> q -> IO Int64
executeFunction func args = do
    conn <- connection
    result <- execute conn func args
    close conn

    pure result

toArray :: Show a => [a] -> String
toArray vs = "{" ++ intercalate "," (map show vs) ++ "}"

