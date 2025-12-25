-- File: src/TeamSpeak/Database.hs
{-# LANGUAGE OverloadedStrings #-}

-- | Module for handling SQLite database connections and schema.
module TeamSpeak.Database
    ( Connection -- Export the Connection type from sqlite-simple
    , openConnection
    , closeConnection
    , createSessionTableQuery -- Export the query string for potential inspection
    , ensureSchema
    , insertSession -- Export the new insertion function
    ) where

import Database.SQLite.Simple (Connection, Query, open, close, execute_, execute)
import Data.Text (Text, pack) -- Import Text if not already needed for Query
import TeamSpeak.Types (UserSession(..)) -- Import the UserSession type and its fields
import Data.Time.Format (formatTime, defaultTimeLocale) -- Import for time formatting
import Data.Time (UTCTime) -- Import UTCTime
import Data.Time.Format.ISO8601 (iso8601Show) -- Import a function to format UTCTime as ISO 8601 string

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

-- | SQL query for inserting a new session.
--   Uses placeholders (?) for values to prevent SQL injection.
insertSessionQuery :: Query
insertSessionQuery =
    "INSERT INTO user_sessions (client_id, client_name, connect_time, disconnect_time) VALUES (?, ?, ?, ?);"

-- | Inserts a 'UserSession' record into the 'user_sessions' table.
--   The 'disconnect_time' field is inserted as NULL if it is 'Nothing'.
insertSession :: Connection -> UserSession -> IO () -- Changed return type to () for simplicity, can be Int64 if row ID is needed
insertSession conn session = do
    let clientId = sessionClientId session
        clientName = sessionClientName session
        connectTime = sessionConnectTime session
        disconnectTimeMaybe = sessionDisconnectTime session
        -- Format UTCTime to ISO 8601 string for database storage
        connectTimeStr = pack $ iso8601Show connectTime
        -- Format disconnect time or use Nothing for NULL
        disconnectTimeStrMaybe = case disconnectTimeMaybe of
                                    Nothing -> Nothing
                                    Just dt -> Just $ pack $ iso8601Show dt

    -- Use 'execute' with a tuple of values. sqlite-simple handles Maybe Text -> NULL conversion.
    execute conn insertSessionQuery (clientId, clientName, connectTimeStr, disconnectTimeStrMaybe)
    -- If the actual row ID of the inserted row is needed, one could use 'lastInsertRowId' from Database.SQLite.Simple after the execute call.
    -- For now, returning () is sufficient for the insertion action itself.