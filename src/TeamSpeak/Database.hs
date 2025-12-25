-- File: src/TeamSpeak/Database.hs
{-# LANGUAGE OverloadedStrings #-}

-- | Module for handling SQLite database connections and schema.
module TeamSpeak.Database
    ( Connection -- Export the Connection type from sqlite-simple
    , openConnection
    , closeConnection
    , createSessionTableQuery -- Export the query string for potential inspection
    , ensureSchema
    ) where

import Database.SQLite.Simple (Connection, Query, open, close, execute_)
import Data.Text (Text) -- Import Text if not already needed for Query

-- | Opens a connection to an SQLite database file.
--   The file will be created if it does not exist.
openConnection :: FilePath -> IO Connection
openConnection = open

-- | Closes the given SQLite database connection.
closeConnection :: Connection -> IO ()
closeConnection = close

-- | The SQL query string to create the `user_sessions` table.
--   Uses TEXT for timestamps as recommended for ISO 8601 strings.
createSessionTableQuery :: Query
createSessionTableQuery = 
    "CREATE TABLE IF NOT EXISTS user_sessions ( \
    \    id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \    client_id INTEGER NOT NULL, \
    \    client_name TEXT NOT NULL, \
    \    connect_time TEXT NOT NULL, \
    \    disconnect_time TEXT \
    \);"

-- | Ensures the `user_sessions` table exists in the database.
--   Creates the table if it does not already exist.
ensureSchema :: Connection -> IO ()
ensureSchema conn = execute_ conn createSessionTableQuery