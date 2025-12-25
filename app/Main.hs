-- File: app/Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative -- Import optparse-applicative
import qualified Data.Text as T -- Import Text for handling the raw log line
import qualified Data.Text.IO as TIO -- Import for readFile
import Data.Time (getCurrentTime) -- Import for time handling if needed later, though not directly used here for parsing
import Control.Monad (forM_) -- Import forM_ for looping over lines

-- Import types and functions from your library
import TeamSpeak.Types (ConnectionEvent(..), ConnectionEventType(..), Client(..))
import TeamSpeak.Parser (connectionEventParser) -- Import the main parser
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

  -- Read the log file content as Text
  logContent <- TIO.readFile (optLogFile opts)

  -- Split the content into lines
  let logLines = T.lines logContent

  -- Iterate over each line and attempt to parse it
  forM_ logLines $ \line -> do
    -- Skip empty lines
    if T.null line
      then return () -- Do nothing for empty lines
      else do
        -- Run the parser on the line
        let result = parse connectionEventParser "log line" line
        case result of
          Left bundle -> do
            -- Print parse errors (optional, can be removed later)
            -- putStrLn $ "Parse error on line: " ++ show line
            -- putStrLn $ errorBundlePretty bundle
            return () -- Silently ignore parse errors for now, just continue to next line
          Right event -> do
            -- Print the successfully parsed event
            putStrLn $ "Parsed event: " ++ show event

  putStrLn "Log parsing completed."