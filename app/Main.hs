-- app/Main.hs
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [logFile] -> do
      putStrLn $ "Parsing: " ++ logFile
      -- TODO: read file, parse lines, insert into DB
    _ -> do
      putStrLn "Usage: teamspeak-server-logparser <logfile>"
      exitFailure
