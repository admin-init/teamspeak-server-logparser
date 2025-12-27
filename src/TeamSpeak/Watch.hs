-- File: src/TeamSpeak/Watch.hs
{-# LANGUAGE OverloadedStrings #-}

-- | Module for watching a directory for log file changes using fsnotify.
module TeamSpeak.Watch
    ( watchLogDirectory
    ) where

import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

-- | Watch a directory for file modification events.
--   When a file is modified, the callback is called with its full path.
--   This function blocks indefinitely.
watchLogDirectory
    :: FilePath                -- ^ Directory to watch
    -> (FilePath -> IO ())     -- ^ Callback: file path that was modified
    -> IO ()
watchLogDirectory logDir onFileModified = do
    putStrLn $ "Starting to watch directory: " ++ logDir
    withManager $ \mgr -> do
        _ <- watchDir mgr logDir shouldHandle handleEvent
        putStrLn "File watcher active. Press Ctrl+C to stop."
        forever (threadDelay maxBound) -- keep alive
  where
    -- Handle both new files and modifications
    shouldHandle (Modified _ _ _) = True
    shouldHandle (Added    _ _ _) = True
    shouldHandle _                = False

    handleEvent (Modified path _ _) = onFileModified path
    handleEvent (Added    path _ _) = onFileModified path
    handleEvent _                   = return ()