-- File: src/TeamSpeak/Database.hs

-- | Module for handling SQLite database connections.
module TeamSpeak.Database
    ( Connection -- Export the Connection type from sqlite-simple
    , openConnection
    , closeConnection
    ) where

import Database.SQLite.Simple (Connection, open, close)

-- | Opens a connection to an SQLite database file.
--   The file will be created if it does not exist.
openConnection :: FilePath -> IO Connection
openConnection = open

-- | Closes the given SQLite database connection.
closeConnection :: Connection -> IO ()
closeConnection = close