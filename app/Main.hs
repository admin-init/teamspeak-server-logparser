-- File: app/Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative -- Import optparse-applicative
import Control.Monad (forM_) -- Import forM_ for looping over lines
import Data.Time (getCurrentTime, UTCTime) -- Import UTCTime and getCurrentTime for session times
import qualified Data.Map.Strict as Map -- Import Map for tracking online sessions
import qualified Data.Text as T -- Import Text for handling the raw log line
import qualified Data.Text.IO as TIO -- Import for readFile
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Map.Strict (Map) -- Import Map type alias
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import System.Directory (listDirectory, doesDirectoryExist)

-- Import types and functions from your library
import TeamSpeak.Types (ConnectionEvent(..), ConnectionEventType(..), Client(..), UserSession(..))
import TeamSpeak.Parser (connectionEventParser) -- Import the main parser
import TeamSpeak.Database 
  ( Connection
  , openConnection
  , closeConnection
  , ensureSchema
  , insertSession
  , updateSessionDisconnectTime
  , execute
  , lastInsertRowId
  ) -- Import DB functions
import TeamSpeak.RealtimeProcessor (startRealtimeProcessingWithSessionTracking)
import Text.Megaparsec (parse, errorBundlePretty) -- Import for running the parser and error handling
import Text.Megaparsec.Char (newline) -- Import newline if needed for parsing, though not directly here
import Data.Void (Void) -- Import Void for the parser's error type

-- | Data structure to hold parsed command-line options.
data Options = BatchOptions
  { optLogFile :: FilePath
  , optDbPath  :: FilePath
  }
 | WatchOptions
  { optLogDir      :: FilePath
  , optDbPath      :: FilePath
  , optOffsetsFile :: FilePath
  , optStateFile   :: FilePath -- NEW: for online session state
  }

-- | Parser for command-line options.
optionsParser :: Parser Options
optionsParser =
  subparser
    ( command "batch"
      ( info batchParser
        ( progDesc "Run one-time batch import from a single log file" )
      )
   <> command "watch"
      ( info watchParser
        ( progDesc "Watch a directory for log changes in real-time" )
      )
    )

batchParser :: Parser Options
batchParser = BatchOptions
  <$> strOption (long "log-file" 
              <> short 'l' 
              <> metavar "FILE" 
              <> help "Input log file"
              )
  <*> strOption (long "db-path"  
              <> short 'd' 
              <> metavar "DB"  
              <> help "SQLite database path" 
              <> value "./sessions.db"
              )

watchParser :: Parser Options
watchParser = WatchOptions
    <$> strOption (long "log-dir"     -- Long option name: --log-file
                <> short 'L'          -- Short option name: -l
                <> metavar "DIR"      -- Meta variable name for help text
                <> help "Log directory to watch"  -- Help text
                <> value "./logs"
                )
    <*> strOption (long "db-path"     -- Long option name: --db-path
                <> short 'd'          -- Short option name: -d
                <> metavar "DB"       -- Meta variable name for help text
                <> help "SQLite database path"    -- Help text
                <> value "./sessions.db"
                )
    <*> strOption (long "offsets"     
                <> short 'o' 
                <> metavar "FILE" 
                <> help "Offset tracking file" 
                <> value "./offsets.json"
                )
    <*> strOption (long "session-state" 
                <> short 's' 
                <> metavar "FILE" 
                <> help "Online session state file" 
                <> value "./online-sessions.json"
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
  case opts of
    BatchOptions logFile dbPath -> runBatch logFile dbPath
    WatchOptions logDir dbPath offsetsFile stateFile -> runWatch logDir dbPath offsetsFile stateFile

runBatch :: FilePath -> FilePath -> IO ()
runBatch logFile dbPath = do
    putStrLn $ "Batch mode: " ++ logFile
    conn <- openConnection dbPath
    ensureSchema conn
    logContent <- TIO.readFile logFile
    let logLines = T.lines logContent
    processLogLines conn Map.empty logLines
    closeConnection conn
    putStrLn "Batch processing completed."

runWatch :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runWatch logDir dbPath offsetsFile stateFile = do
    putStrLn $ "Watch mode: monitoring " ++ logDir
    startRealtimeProcessingWithSessionTracking offsetsFile stateFile logDir dbPath

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