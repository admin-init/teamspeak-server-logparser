-- File: src/TeamSpeak/RealtimeProcessor.hs
{-# LANGUAGE OverloadedStrings #-}

-- | High-level real-time log processing loop with offset tracking.
module TeamSpeak.RealtimeProcessor
    ( startRealtimeProcessing
    ) where

import TeamSpeak.Watch (watchLogDirectory)
import TeamSpeak.Offsets (readLogOffsets, writeLogOffsets, LogOffsets, emptyLogOffsets)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
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
import Data.Text.Encoding (decodeUtf8')
import Data.Text (unpack)
import GHC.IO.Handle (hGetContents)

-- | Start real-time processing of log files in a directory.
--   Reads from 'offsetsFile' on startup, updates it periodically.
startRealtimeProcessing
    :: FilePath          -- ^ Path to offsets.json
    -> FilePath          -- ^ Log directory to watch
    -> (FilePath -> Integer -> IO Integer) -- ^ Callback: (file, oldOffset) -> IO newOffset
    -> IO ()
startRealtimeProcessing offsetsFile logDir processFileIncrement = do
    -- Load initial offsets
    initialOffsets <- readLogOffsets offsetsFile
    putStrLn $ "Loaded offsets: " ++ show (Map.toList initialOffsets)

    -- Use MVar to safely share and update offsets across threads
    offsetsVar <- newMVar initialOffsets

    -- Start background thread to persist offsets every 5 seconds
    _ <- forkIO $ persistOffsetsLoop offsetsFile offsetsVar

    -- Define the file modification handler
    let onFileModified fullPath = do
            newOffsets <- withMVar offsetsVar $ \currentOffsets -> do
                let currentOffset = Map.findWithDefault 0 fullPath currentOffsets
                putStrLn $ "Processing " ++ fullPath ++ " from offset " ++ show currentOffset
                newOffset <- processFileIncrement fullPath currentOffset
                let updatedOffsets = Map.insert fullPath newOffset currentOffsets
                return updatedOffsets
            -- Update the shared state
            swapMVar offsetsVar newOffsets
            return ()

    -- Start watching
    watchLogDirectory logDir onFileModified

-- | Periodically write offsets to disk (every 5 seconds)
persistOffsetsLoop :: FilePath -> MVar LogOffsets -> IO ()
persistOffsetsLoop offsetsFile offsetsVar = do
    threadDelay (5 * 1000000) -- 5 seconds
    offsets <- readMVar offsetsVar
    handle (\(e :: SomeException) -> putStrLn $ "Warning: Failed to write offsets: " ++ show e) $
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
            return (content, offset + fromIntegral (BS.length content))

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