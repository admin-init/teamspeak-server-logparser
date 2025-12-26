-- File: test/TeamSpeak/DatabaseSpec.hs
{-# LANGUAGE OverloadedStrings #-}

module TeamSpeak.DatabaseSpec where

import Test.Hspec
import Database.SQLite.Simple (Connection, query, query_, Only(..))
import qualified Data.Text as T
import System.Directory (removeFile, doesFileExist)
import System.IO.Temp (withSystemTempFile) -- Import from tempfile
import TeamSpeak.Database (ensureSchema, openConnection, closeConnection)

-- | Helper function to check if a table exists in the SQLite database.
--   Queries the 'sqlite_master' system table.
tableExists :: Connection -> String -> IO Bool
tableExists conn tableName = do
    results <- query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only (T.pack tableName))
    return (not (null (results :: [Only T.Text])))

spec :: Spec
spec = do
    describe "ensureSchema" $ do
        it "creates the 'user_sessions' table if it does not exist" $ do
            -- Use a temporary file for the test database
            withSystemTempFile "test_db" $ \tempFilePath _handle -> do
                -- Ensure the temp file is deleted after the test (handle might be closed by withSystemTempFile)
                -- Open connection to the temporary database file
                conn <- openConnection tempFilePath

                -- Verify table does not exist *before* calling ensureSchema
                -- This is a good practice to ensure the test is meaningful
                tableShouldNotExistBefore <- tableExists conn "user_sessions"
                tableShouldNotExistBefore `shouldBe` False

                -- Call ensureSchema to create the table
                ensureSchema conn

                -- Verify table exists *after* calling ensureSchema
                tableShouldExistAfter <- tableExists conn "user_sessions"
                tableShouldExistAfter `shouldBe` True

                -- Close the database connection
                closeConnection conn

                -- Optionally, delete the temp file after closing the connection
                -- This is often handled by 'withSystemTempFile', but good to be explicit if needed
                -- The _handle is the temp file handle, which 'withSystemTempFile' manages.
                -- We only have the path here. The file should be automatically cleaned up
                -- when the action inside 'withSystemTempFile' finishes, but if you need
                -- to ensure deletion *after* connection close, you can do it here.
                -- However, 'withSystemTempFile' usually handles this.
                -- removeFile tempFilePath -- Uncomment if explicit deletion after close is desired and not handled by withSystemTempFile

        -- Optional: Add more tests here for other schema aspects if needed
        -- e.g., checking column types, constraints, etc., though this is more complex.
