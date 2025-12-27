-- File: src/TeamSpeak/RealtimeProcessor.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-} 
{-# LANGUAGE DeriveGeneric #-}

-- | High-level real-time log processing loop with offset tracking.
module TeamSpeak.RealtimeProcessor
    ( startRealtimeProcessingWithSessionTracking
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
import TeamSpeak.Database 
    ( Connection
    , openConnection
    , ensureSchema
    , insertSession
    , lastInsertRowId
    , updateSessionDisconnectTime
    )
import Text.Megaparsec (parseMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson.Types as Aeson
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
import Control.Monad (foldM)
import Data.Aeson 
    ( encode
    , decodeStrict
    , ToJSON(toJSON)
    , FromJSON(parseJSON)
    )
import Data.Text (unpack)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Int (Int64)
import Data.Time (UTCTime) -- Import UTCTime
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

-- | Online session state: maps ClientId to (SessionId, ConnectTime)
type OnlineSessions = Map.Map Int (Int64, UTCTime)

-- | Persistable record for session state
data SessionStateRecord = SessionStateRecord
    { ssrSessionId   :: !Int64
    , ssrConnectTime :: !UTCTime
    } deriving (Show, Generic, ToJSON, FromJSON)

-- | Save online sessions to JSON file
saveOnlineSessions :: FilePath -> OnlineSessions -> IO ()
saveOnlineSessions path sessions = do
    let jsonMap = Map.map (\(sid, ct) -> SessionStateRecord sid ct) sessions
    LBS.writeFile path (encode jsonMap)

-- | Load online sessions from JSON file
loadOnlineSessions :: FilePath -> IO OnlineSessions
loadOnlineSessions path = do
    exists <- doesFileExist path
    if not exists
        then return Map.empty
        else do
            bs <- BS.readFile path
            case decodeStrict bs of
                Nothing -> do
                    putStrLn $ "Warning: Failed to decode " ++ path ++ ", starting fresh"
                    return Map.empty
                Just (jsonMap :: Map.Map Int SessionStateRecord) ->
                    return $ Map.map (\(SessionStateRecord sid ct) -> (sid, ct)) jsonMap

-- | Start real-time processing of log files in a directory.
--   Reads from 'offsetsFile' on startup, updates it periodically.
startRealtimeProcessingWithSessionTracking
    :: FilePath -- offsets.json
    -> FilePath -- online-sessions.json
    -> FilePath -- log dir
    -> FilePath -- db
    -> IO ()


startRealtimeProcessingWithSessionTracking offsetsFile stateFile logDir dbPath = do
    conn <- openConnection dbPath
    ensureSchema conn

    -- Load initial offsets
    initialOffsets <- readLogOffsets offsetsFile
    initialSessions <- loadOnlineSessions stateFile

    -- Shared state for offsets
    offsetsVar <- newMVar initialOffsets
    sessionsVar <- newMVar initialSessions

    -- Start background persistence
    _ <- forkIO $ persistOffsetsLoop offsetsFile offsetsVar
    _ <- forkIO $ persistSessionsLoop stateFile sessionsVar

    -- Define the file modification handler
    let onFileModified fullPath = do
            putStrLn $ ">>> Detected change in: " ++ fullPath  -- DEBUG
            (newOffsets, newSessions) <- withMVar offsetsVar $ \curOffsets ->
                withMVar sessionsVar $ \curSessions -> do
                    let offset = Map.findWithDefault 0 fullPath curOffsets
                    (content, fileSize) <- readFileFromOffset fullPath offset
                    putStrLn $ ">>> Read " ++ show (BS.length content) ++ " bytes from offset " ++ show offset
                    if BS.null content
                        then return (curOffsets, curSessions)
                        else do
                            let text = TE.decodeUtf8With lenientDecode content
                            putStrLn $ ">>> Content: " ++ take 100 (unpack text) ++ "..."
                            let events = mapMaybe (parseMaybe connectionEventParser) (T.lines text)
                            (updatedSessions, _) <- foldM (processEvent conn) (curSessions, fullPath) events
                            return (Map.insert fullPath fileSize curOffsets, updatedSessions)
            swapMVar offsetsVar newOffsets
            swapMVar sessionsVar newSessions
            return ()

    -- Start watching
    watchLogDirectory logDir onFileModified

-- | Process one event, update session state
processEvent
    :: Connection
    -> (OnlineSessions, FilePath)
    -> ConnectionEvent
    -> IO (OnlineSessions, FilePath)
processEvent conn (sessions, filePath) event = do
    case eventType event of
        Connected -> do
            let client = eventClient event
            let session = UserSession (-1) (clientId client) (clientName client) (eventTimestamp event) Nothing
            putStrLn $ ">>> About to insert session for " ++ show (clientName client)
            insertSession conn session
            sid <- lastInsertRowId conn
            putStrLn $ ">>> Inserted with rowid: " ++ show sid
            let newSessions = Map.insert (clientId client) (sid, eventTimestamp event) sessions
            return (newSessions, filePath)
        Disconnected -> do
            let cid = clientId (eventClient event)
            case Map.lookup cid sessions of
                Nothing -> do
                    putStrLn $ "Warning: Disconnect for unknown client " ++ show cid ++ " in " ++ filePath
                    return (sessions, filePath)
                Just (sid, _) -> do
                    putStrLn $ ">>> Updating disconnect time for session " ++ show sid
                    updateSessionDisconnectTime conn sid (eventTimestamp event)
                    let newSessions = Map.delete cid sessions
                    return (newSessions, filePath)


-- | Periodically write offsets to disk (every 5 seconds)
persistOffsetsLoop :: FilePath -> MVar LogOffsets -> IO ()
persistOffsetsLoop f v = do
    threadDelay (750000)   -- 0.75 * 1000000 = 0.75 seconds
    offsets <- readMVar v
    handle (\(e :: SomeException) -> print e) $ writeLogOffsets f offsets
    persistOffsetsLoop f v

persistSessionsLoop :: FilePath -> MVar OnlineSessions -> IO ()
persistSessionsLoop f v = do
    threadDelay (750000)
    sessions <- readMVar v
    handle (\(e :: SomeException) -> print e) $ saveOnlineSessions f sessions
    persistSessionsLoop f v

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