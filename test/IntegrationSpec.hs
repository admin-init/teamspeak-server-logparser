-- File: test/IntegrationSpec.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module IntegrationSpec (spec) where

import Test.Hspec
import System.Directory
    ( removePathForcibly
    , createDirectoryIfMissing
    , listDirectory
    , doesFileExist
    , getTemporaryDirectory
    , makeAbsolute
    )
import System.IO.Temp (createTempDirectory)
import System.FilePath ((</>))
import System.Process
    ( readCreateProcessWithExitCode
    , createProcess
    , readProcess
    , terminateProcess
    , waitForProcess
    , CreateProcess(..)
    , proc
    , cwd
    )
import System.Exit (ExitCode(ExitSuccess))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import Control.Exception (bracket_)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Database.SQLite.Simple
    ( open
    , close
    , query_
    )
import qualified Data.Map.Strict as Map

import TeamSpeak.Types (UserSession(..))
import TeamSpeak.Database (ensureSchema)

-- | Run the CLI via 'cabal run' in the project root (where cabal.project exists)
runCli :: [String] -> IO (ExitCode, String, String)
runCli args = readCreateProcessWithExitCode (proc "cabal" ("exec" : "teamspeak-server-logparser-cli" : "--" : args)) ""

-- Note: The path above assumes standard cabal build layout.
-- In CI or different setups, you might prefer `stack exec -- ...` or just `teamspeak-server-logparser-cli`
-- For portability, consider using `lookupEnv "CLI_PATH"` or building via `cabal run` in test script.

spec :: Spec
spec = do
    describe "Batch mode" $ do
        it "parses a log file and inserts sessions into DB" $
            withTempDirTest $ \tmpDir -> do
                let logFile = tmpDir </> "test.log"
                    dbFile  = tmpDir </> "sessions.db"
                writeFile logFile
                    "2025-12-26 10:00:00.000000|INFO    |VirtualServerBase|1  |client connected 'Alice'(id:123) using a myTeamSpeak ID from 127.0.0.1:1234\n\
                    \2025-12-26 10:05:00.000000|INFO    |VirtualServerBase|1  |client disconnected 'Alice'(id:123) reason 'left'\n\
                    \2025-12-26 10:10:00.000000|INFO    |VirtualServerBase|1  |client connected 'Bob'(id:456) using a myTeamSpeak ID from 127.0.0.1:5678\n"

                absLogFile <- makeAbsolute logFile
                absDbFile  <- makeAbsolute dbFile

                -- Pass ABSOLUTE paths, and NO tmpDir argument
                (ec, out, err) <- runCli ["batch", "-l", absLogFile, "-d", absDbFile]
                putStrLn "=== CLI STDOUT ==="
                putStrLn out
                putStrLn "=== CLI STDERR ==="
                putStrLn err
                ec `shouldBe` ExitSuccess
                out `shouldContain` "Batch processing completed"

                conn <- open absDbFile  -- use absolute path
                rows :: [UserSession] <- query_ conn "SELECT id, client_id, client_name, connect_time, disconnect_time FROM user_sessions ORDER BY client_id"                
                close conn

                -- DEBUG: print actual DB content BEFORE assertion
                dbDump <- readProcess "sqlite3" [absDbFile, "SELECT * FROM user_sessions;"] ""
                putStrLn "=== ACTUAL DB CONTENT ==="
                putStr dbDump

                rows `shouldBe`
                    [ UserSession 1 123 "Alice" (read "2025-12-26 10:00:00 UTC") (Just $ read "2025-12-26 10:05:00 UTC")
                    , UserSession 2 456 "Bob"   (read "2025-12-26 10:10:00 UTC") Nothing
                    ]

    describe "Watch mode" $ do
        it "watches a directory, processes new lines, and persists state" $
            withTempDirTest $ \tmpDir -> do
                let logDir       = tmpDir </> "logs"
                    dbFile       = tmpDir </> "sessions.db"
                    offsetsFile  = tmpDir </> "offsets.json"
                    stateFile    = tmpDir </> "online-sessions.json"
                    logFile      = logDir </> "server_1.log"

                createDirectoryIfMissing True logDir

                absLogDir      <- makeAbsolute logDir
                absDbFile      <- makeAbsolute dbFile
                absOffsetsFile <- makeAbsolute offsetsFile
                absStateFile   <- makeAbsolute stateFile
                absLogFile     <- makeAbsolute logFile

                let args = [ "exec", "teamspeak-server-logparser-cli", "--"
                        , "watch", "-L", absLogDir, "-d", absDbFile, "-o", absOffsetsFile, "-s", absStateFile
                        ]
                (_, _, _, ph) <- createProcess (proc "cabal" args)

                putStrLn $ ">>> Using DB: " ++ absDbFile

                -- Wait for watcher to initialize
                threadDelay 2000000

                -- Now write logs AFTER watcher is running
                appendFile logFile "2025-12-26 11:00:00.000000|INFO    |VirtualServerBase|1  |client connected 'Carol'(id:789) using a myTeamSpeak ID from 127.0.0.1:9999\n"
                threadDelay 500000

                appendFile logFile "2025-12-26 11:02:00.000000|INFO    |VirtualServerBase|1  |client disconnected 'Carol'(id:789) reason 'timeout'\n"
                threadDelay 2000000  -- give time to process disconnect

                terminateProcess ph
                _ <- waitForProcess ph

                conn <- open absDbFile  -- absolute
                rows :: [UserSession] <- query_ conn "SELECT id, client_id, client_name, connect_time, disconnect_time FROM user_sessions"                
                close conn

                putStrLn $ ">>> Test checking DB at: " ++ absDbFile
                dbDump <- readProcess "sqlite3" [absDbFile, "SELECT * FROM user_sessions;"] ""
                putStrLn "=== ACTUAL DB CONTENT ==="
                putStr dbDump

                length rows `shouldBe` 1
                let sess = head rows
                sessionId sess `shouldSatisfy` (> 0)
                sessionClientId sess `shouldBe` 789
                sessionClientName sess `shouldBe` "Carol"
                sessionConnectTime sess `shouldBe` read "2025-12-26 11:00:00 UTC"
                sessionDisconnectTime sess `shouldBe` Just (read "2025-12-26 11:02:00 UTC")

                offsets <- waitForOffsets absOffsetsFile absLogFile
                Map.lookup absLogFile offsets `shouldSatisfy` (> Just 0)

                stateExists <- doesFileExist absStateFile
                stateExists `shouldBe` True
                stateBs <- LBS.readFile absStateFile
                case Aeson.decode stateBs of
                    Nothing -> expectationFailure "Failed to parse online-sessions.json"
                    Just (state :: Map.Map Int (Aeson.Object)) ->
                        Map.null state `shouldBe` True

withTempDirTest :: (FilePath -> IO ()) -> IO ()
withTempDirTest action = do
    tmpDir <- getTemporaryDirectory >>= \td -> createTempDirectory td "tslog-test"
    bracket_ (pure ()) (removePathForcibly tmpDir) (action tmpDir)

waitForOffsets :: FilePath -> FilePath -> IO (Map.Map FilePath Integer)
waitForOffsets offsetsFile logFile = do
    start <- getCurrentTime
    let loop = do
            exists <- doesFileExist offsetsFile
            if not exists
                then do
                    now <- getCurrentTime
                    if diffUTCTime now start > 6
                        then error "Timeout waiting for offsets.json"  -- ✅ throws IOError, but test fails as expected
                        else threadDelay 200000 >> loop
                else do
                    bs <- LBS.readFile offsetsFile
                    case Aeson.decode bs :: Maybe (Map.Map FilePath Integer) of
                        Nothing -> threadDelay 200000 >> loop
                        Just offsets ->
                            if Map.lookup logFile offsets > Just 0
                                then return offsets
                                else threadDelay 200000 >> loop
    loop