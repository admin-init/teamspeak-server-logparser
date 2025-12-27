-- File: src/TeamSpeak/Offsets.hs
{-# LANGUAGE OverloadedStrings #-}

-- | Module for reading and writing log file offset state to/from JSON.
module TeamSpeak.Offsets
    ( readLogOffsets
    , writeLogOffsets
    , LogOffsets
    , emptyLogOffsets
    ) where

import TeamSpeak.Types (LogOffsets, emptyLogOffsets)
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)
import Control.Exception (catch, SomeException)

-- | Read 'LogOffsets' from a JSON file. Returns empty map if file doesn't exist or is invalid.
readLogOffsets :: FilePath -> IO LogOffsets
readLogOffsets jsonPath = do
    exists <- doesFileExist jsonPath
    if not exists
        then return emptyLogOffsets
        else do
            eContent <- tryReadFile jsonPath
            case eContent of
                Left _ -> do
                    putStrLn $ "Warning: Failed to parse offsets file '" ++ jsonPath ++ "'. Using empty offsets."
                    return emptyLogOffsets
                Right bs -> case eitherDecode bs of
                    Left err -> do
                        putStrLn $ "Warning: JSON decode error in offsets file: " ++ err
                        return emptyLogOffsets
                    Right offsets -> return offsets

-- | Write 'LogOffsets' to a JSON file (lazy bytestring).
writeLogOffsets :: FilePath -> LogOffsets -> IO ()
writeLogOffsets jsonPath offsets = BL.writeFile jsonPath (encode offsets)

-- Helper: safely read file, catching all exceptions
tryReadFile :: FilePath -> IO (Either SomeException BL.ByteString)
tryReadFile path = catch (Right <$> BL.readFile path) (return . Left)