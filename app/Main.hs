-- File: app/Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative -- Import optparse-applicative
import qualified Data.Text as T -- Import Text for handling the raw log line
import qualified Data.Text.IO as TIO -- Import for readFile
import Control.Monad (forM_) -- Import forM_ for looping over lines
import Data.Time (getCurrentTime, UTCTime) -- Import UTCTime and getCurrentTime for session times
import qualified Data.Map.Strict as Map -- Import Map for tracking online sessions
import Data.Map.Strict (Map) -- Import Map type alias
import Data.Int (Int64)

-- Import types and functions from your library
import TeamSpeak.Types (ConnectionEvent(..), ConnectionEventType(..), Client(..), UserSession(..))
import TeamSpeak.Parser (connectionEventParser) -- Import the main parser
import TeamSpeak.Database (Connection, openConnection, closeConnection, ensureSchema, insertSession, updateSessionDisconnectTime, execute, lastInsertRowId) -- Import DB functions
import Text.Megaparsec (parse, errorBundlePretty) -- Import for running the parser and error handling
import Text.Megaparsec.Char (newline) -- Import newline if needed for parsing, though not directly here
import Data.Void (Void) -- Import Void for the parser's error type

-- | Data structure to hold parsed command-line options.
data Options = Options
  { optLogFile :: FilePath
  , optDbPath  :: FilePath
  } deriving (Show, Eq)

-- | Parser for command-line options.
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption -- Parse a string option
      ( long "log-file" -- Long option name: --log-file
     <> short 'l'       -- Short option name: -l
     <> metavar "FILEPATH" -- Meta variable name for help text
     <> help "Path to the TeamSpeak server log file" -- Help text
      )
  <*> strOption -- Parse another string option
      ( long "db-path" -- Long option name: --db-path
     <> short 'd'      -- Short option name: -d
     <> metavar "DBPATH" -- Meta variable name for help text
     <> help "Path to the SQLite database file" -- Help text
      )

-- | Type alias for tracking connected sessions: Map ClientId SessionId
type ConnectedSessions = Map Int Int64

-- | Main entry point of the executable.
main :: IO ()
main = do
  -- Parse command-line arguments using the defined parser
  opts <- execParser (info (optionsParser <**> helper) -- Combine parser with helper (adds --help)
                      ( fullDesc -- Provide full description
                     <> progDesc "Parse TeamSpeak logs and store session data in an SQLite database" -- Program description
                     <> header "teamspeak-server-logparser-cli - A log parser for TeamSpeak servers" -- Header for help text
                      )
                     )
  -- Print the parsed options for now (as a placeholder)
  putStrLn $ "Log file: " ++ optLogFile opts
  putStrLn $ "Database path: " ++ optDbPath opts
  putStrLn "Placeholder: Actual processing would happen here."

  -- Open database connection
  conn <- openConnection (optDbPath opts)
  -- Ensure the schema exists
  ensureSchema conn

  -- Initialize the map to track connected sessions
  let initialConnectedSessions = Map.empty :: ConnectedSessions

  -- Read the log file content as Text
  logContent <- TIO.readFile (optLogFile opts)
  -- Split the content into lines
  let logLines = T.lines logContent

  -- Process lines iteratively, passing the state of connected sessions
  -- We need a stateful way to process lines and update the map.
  -- A simple fold might not be sufficient if we need to carry state through IO actions easily.
  -- Using a recursive helper function or a state monad would be cleaner for complex state.
  -- For now, let's use a simple recursive approach or a loop with mutable state.
  -- We'll use a helper function that takes the connection, the map of connected sessions, and the list of lines.
  processLogLines conn initialConnectedSessions logLines

  -- Close the database connection after processing
  closeConnection conn
  putStrLn "Log parsing and database storage completed."

-- | Helper function to process log lines recursively, maintaining the state of connected sessions.
processLogLines :: Connection -> ConnectedSessions -> [T.Text] -> IO ()
processLogLines _conn connectedSessions [] = return ()
processLogLines conn connectedSessions (line:remainingLines) = do
  nextConnectedSessions <-
    if T.null line
      then return connectedSessions
      else processSingleLine conn connectedSessions line
  processLogLines conn nextConnectedSessions remainingLines

-- | Helper function to process a single log line and update the connected sessions map accordingly.
processSingleLine :: Connection -> ConnectedSessions -> T.Text -> IO ConnectedSessions
processSingleLine conn connectedSessions line = do
  let result = parse connectionEventParser "log line" line
  case result of
    Left bundle -> do
      -- Silently ignore parse errors for now, return map unchanged
      return connectedSessions
    Right event -> do
      case eventType event of
        Connected -> do
          -- Create initial UserSession (disconnect_time = Nothing)
          let session = userSessionFromConnectedEvent event
          -- Insert it using the shared database function
          insertSession conn session
          -- Get the auto-generated ID
          newSessionId <- lastInsertRowId conn
          -- Add to tracking map
          let updatedMap = Map.insert (clientId $ eventClient event) newSessionId connectedSessions
          return updatedMap
        Disconnected -> do
          let clientIdToMatch = clientId $ eventClient event
          -- Check if the client was in the connected map
          case Map.lookup clientIdToMatch connectedSessions of
            Nothing -> do
              -- Client was not found in the map, might have connected before this log started,
              -- or there was a duplicate disconnect, or a log parsing issue.
              -- For now, just print a warning and return the map unchanged.
              putStrLn $ "Warning: Disconnected event for client not found in online map: " ++ show clientIdToMatch
              return connectedSessions
            Just sessionId -> do
              -- Found the session ID, update the disconnect time in the database
              updateSessionDisconnectTime conn sessionId (eventTimestamp event)
              -- Remove the client from the connected map
              let updatedMap = Map.delete clientIdToMatch connectedSessions
              -- Optional: Print confirmation
              -- putStrLn $ "Updated session ID " ++ show sessionId ++ " disconnect time for client " ++ clientIdToMatch
              return updatedMap

-- | Helper function to convert a 'ConnectionEvent' (Connected) into an initial 'UserSession'.
--   The 'disconnect_time' is set to 'Nothing'.
userSessionFromConnectedEvent :: ConnectionEvent -> UserSession
userSessionFromConnectedEvent event = UserSession
  { sessionId = -1 -- Placeholder ID, will be auto-generated by the database
  , sessionClientId = clientId $ eventClient event
  , sessionClientName = clientName $ eventClient event
  , sessionConnectTime = eventTimestamp event
  , sessionDisconnectTime = Nothing -- Initial value for a connected session
  }