{-# LANGUAGE OverloadedStrings #-}

module TeamSpeak.TestRealtime where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import System.IO (Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek), hFileSize, hSeek, hIsEOF, withBinaryFile)
import TeamSpeak.RealtimeProcessor

-- Helper: read all remaining bytes from a handle
hGetRemaining :: Handle -> IO BS.ByteString
hGetRemaining h = go mempty
  where
    go acc = do
      eof <- hIsEOF h
      if eof
        then return acc
        else do
          chunk <- BS.hGet h 4096
          go (acc <> chunk)

-- Simple helper: read file from offset to EOF
readFileFromOffset :: FilePath -> Integer -> IO (BS.ByteString, Integer)
readFileFromOffset path offset = do
  withBinaryFile path ReadMode $ \h -> do
    size <- hFileSize h
    if offset >= size
      then return (mempty, size)
      else do
        hSeek h AbsoluteSeek offset
        content <- hGetRemaining h
        return (content, size)

-- Dummy processor: just print new lines
dummyProcessor :: FilePath -> Integer -> IO Integer
dummyProcessor path offset = do
    (content, newOffset) <- readFileFromOffset path offset
    case TE.decodeUtf8' content of
        Left err -> putStrLn $ "Decode error: " ++ show err
        Right txt -> mapM_ putStrLn (lines (T.unpack txt))
    return newOffset

main :: IO ()
main = startRealtimeProcessing "./offsets.json" "./data" dummyProcessor