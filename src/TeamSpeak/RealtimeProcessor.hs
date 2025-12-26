-- File: src/TeamSpeak/RealtimeProcessor.hs
{-# LANGUAGE OverloadedStrings #-}

-- | High-level real-time log processing loop with offset tracking.
module TeamSpeak.RealtimeProcessor
    ( startRealtimeProcessing
    ) where

import TeamSpeak.Types
  ( UserSession(..)
  , Client (..)
  , ConnectionEvent(..)
  , ConnectionEventType(..)
  )
import TeamSpeak.Watch (watchLogDirectory)
import TeamSpeak.Offsets (readLogOffsets, writeLogOffsets, LogOffsets, emptyLogOffsets)
import TeamSpeak.Parser (connectionEventParser)
import TeamSpeak.Database (openConnection, ensureSchema, insertSession)
import Text.Megaparsec (parseMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO
  ( Handle
  , openBinaryFile
  , hSeek
  , hFileSize
  , hClose
  , hIsEOF
  , IOMode(ReadMode)
  , SeekMode(AbsoluteSeek)
  )
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar, readMVar, swapMVar)
import Control.Exception (handle, SomeException)
import Data.Text (unpack)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Maybe (mapMaybe)
import Database.SQLite.Simple (Connection)

-- | Start real-time processing of log files in a directory.
--   Reads from 'offsetsFile' on startup, updates it periodically.
startRealtimeProcessing
    :: FilePath          -- ^ Path to offsets.json
    -> FilePath          -- ^ Log directory to watch
    -> FilePath          -- ^ SQLite database path
    -> IO ()
startRealtimeProcessing offsetsFile logDir dbPath = do
    conn <- openConnection dbPath
    ensureSchema conn
    putStrLn $ "Database initialized at: " ++ dbPath

    -- Load initial offsets
    initialOffsets <- readLogOffsets offsetsFile
    putStrLn $ "Loaded offsets for " ++ show (length (Map.keys initialOffsets)) ++ " files."

    -- Shared state for offsets
    offsetsVar <- newMVar initialOffsets

    -- Start background persistence
    _ <- forkIO $ persistOffsetsLoop offsetsFile offsetsVar

    -- Define the file modification handler
    let onFileModified fullPath = do
            newOffsets <- withMVar offsetsVar $ \currentOffsets -> do
                let currentOffset = Map.findWithDefault 0 fullPath currentOffsets
                putStrLn $ "[+] Processing " ++ fullPath ++ " from offset " ++ show currentOffset
                newOffset <- processLogFile conn fullPath currentOffset
                return (Map.insert fullPath newOffset currentOffsets)
            swapMVar offsetsVar newOffsets
            return ()

    -- Start watching
    watchLogDirectory logDir onFileModified

-- | Process a log file from given offset: read, parse, insert connected events into DB.
processLogFile :: Connection -> FilePath -> Integer -> IO Integer
processLogFile conn path offset = do
    (content, fileSize) <- readFileFromOffset path offset
    if BS.null content
        then do
            putStrLn $ "[-] No new content in " ++ path
            return fileSize
        else do
            let text = TE.decodeUtf8With lenientDecode content
            let lines' = T.lines text
            putStrLn $ "[+] Read " ++ show (length lines') ++ " new lines from " ++ path

            -- Parse each line into Maybe ConnectionEvent
            let parsedEvents = mapMaybe (parseMaybe connectionEventParser) lines'

            -- Filter only 'Connected' events and convert to UserSession (with disconnect_time = Nothing)
            let sessionsToInsert = 
                  [ UserSession
                      { sessionId = -1  -- placeholder; DB auto-increments
                      , sessionClientId = clientId client
                      , sessionClientName = clientName client
                      , sessionConnectTime = timestamp
                      , sessionDisconnectTime = Nothing
                      }
                  | ConnectionEvent timestamp client Connected <- parsedEvents
                  ]

            putStrLn $ "[+] Inserting " ++ show (length sessionsToInsert) ++ " new sessions"
            mapM_ (insertSession conn) sessionsToInsert

            return fileSize

-- | Periodically write offsets to disk (every 5 seconds)
persistOffsetsLoop :: FilePath -> MVar LogOffsets -> IO ()
persistOffsetsLoop offsetsFile offsetsVar = do
    threadDelay (5 * 1000000) -- 5 seconds
    offsets <- readMVar offsetsVar
    handle (\(e :: SomeException) -> putStrLn $ "Warning: Failed to persist offsets: " ++ show e) $
        writeLogOffsets offsetsFile offsets
    persistOffsetsLoop offsetsFile offsetsVar

-- | Helper: read file content from a given byte offset to end (binary-safe)
readFileFromOffset :: FilePath -> Integer -> IO (BS.ByteString, Integer)
readFileFromOffset path offset = do
    handle <- openBinaryFile path ReadMode
    fileSize <- hFileSize handle
    if offset >= fileSize
        then do
            hClose handle
            return (BS.empty, fileSize)
        else do
            hSeek handle AbsoluteSeek offset
            content <- hGetRemaining handle
            hClose handle
            return (content, fileSize)

-- | Extension of Handle to read all remaining bytes
hGetRemaining :: Handle -> IO BS.ByteString
hGetRemaining h = go BS.empty
  where
    go acc = do
        eof <- hIsEOF h
        if eof
            then return acc
            else do
                chunk <- BS.hGet h 4096
                go (acc <> chunk)