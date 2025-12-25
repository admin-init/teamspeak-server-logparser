-- File: app/Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative -- Import optparse-applicative
import Data.Semigroup ((<>)) -- Import <> for combining options
import qualified Data.Text as T -- Import Text for handling file paths if needed, though String is often fine for file paths in CLI args

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