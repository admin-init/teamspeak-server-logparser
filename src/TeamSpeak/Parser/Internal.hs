-- File: src/TeamSpeak/Parser/Internal.hs
{-# LANGUAGE OverloadedStrings #-}

-- | Internal module containing the low-level Megaparsec parsers for
--   TeamSpeak log events. Exposed to the main Parser module.
module TeamSpeak.Parser.Internal where

import           Control.Monad.Combinators (skipManyTill)
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Data.Char (isDigit)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Void (Void)
import           Data.Time (UTCTime, parseTimeM)
import           Data.Time.Format (defaultTimeLocale)
import           TeamSpeak.Types (Client (..), ConnectionEvent (..), ConnectionEventType (..))

-- | Type alias for our Megaparsec parser using Text as input and Void for error type.
type Parser = Parsec Void Text

-- | Parses the timestamp part of a log line.
--   Expected format: YYYY-MM-DD HH:MM:SS.SSSSSS
--   Uses 'L.lexeme' implicitly via 'L.decimal' and manual parsing for separators.
--   Note: This is a simplified parser. For more robust parsing, consider using attoparsec or
--         building a dedicated time parser with Megaparsec combinators if performance is critical.
--         For now, we'll parse the string and use 'parseTimeM'.
timestampParser :: Parser UTCTime
timestampParser = do
    dateStr <- count 10 (satisfy (\c -> isDigit c || c == '-')) -- YYYY-MM-DD
    _ <- char ' '
    timeStr <- count 15 (satisfy (\c -> isDigit c || c == '.' || c == ':')) -- HH:MM:SS.SSSSSS
    let fullTimeStr = dateStr <> " " <> timeStr
    let formatStr = "%Y-%m-%d %H:%M:%S%Q" -- %Q handles fractional seconds
    maybeTime <- liftMaybe "Failed to parse timestamp" $ parseTimeM True defaultTimeLocale formatStr fullTimeStr
    _ <- char '|' -- Consume the separator after the timestamp
    return maybeTime
  where
    liftMaybe msg Nothing = fail msg
    liftMaybe _ (Just a) = return a

-- | Parses a single-quoted string.
--   Assumes no escaped quotes within the string for simplicity.
quotedStringParser :: Parser Text
quotedStringParser = do
    _ <- char '\''
    content <- takeWhileP (Just "quoted string content") (/= '\'')
    _ <- char '\''
    return content

-- | Parses the client ID part: (id:XXX)
clientIdParser :: Parser Int
clientIdParser = do
    _ <- char '('
    _ <- string "id:"
    clientId <- L.decimal
    _ <- char ')'
    return clientId

-- | Parses the 'client connected' keyword sequence.
clientConnectedKeywordParser :: Parser ()
clientConnectedKeywordParser = do
    _ <- string "client connected "
    return ()

-- | Parses the 'client disconnected' keyword sequence.
clientDisconnectedKeywordParser :: Parser ()
clientDisconnectedKeywordParser = do
    _ <- string "client disconnected "
    return ()

-- | Parses a single client information segment: 'Name'(id:XXX)
--   Assumes no spaces between the quote, name, and ID part.
clientInfoParser :: Parser Client
clientInfoParser = do
    name <- quotedStringParser
    _ <- char '('
    _ <- string "id:"
    idNum <- L.decimal
    _ <- char ')'
    return $ Client idNum name

-- | Main parser for a 'client connected' log line.
--   Returns the parsed ConnectionEvent.
--   Expects format: timestamp|INFO|...|client connected 'Name'(id:XXX) ...
clientConnectedParser :: Parser ConnectionEvent
clientConnectedParser = do
    timestamp <- timestampParser  -- consumes up to and including the first '|'
    -- Now we expect: "INFO    |VirtualServerBase|<digits>  |client connected ..."
    _ <- string "INFO    |VirtualServerBase|"
    _ <- takeWhile1P (Just "server id") (/= ' ')  -- e.g. "1"
    _ <- string "  |"  -- two spaces and a pipe
    _ <- string "client connected "
    client <- clientInfoParser
    -- Optionally consume rest of line (to avoid parse failure if extra text)
    _ <- takeWhileP (Just "trailing") (/= '\n')
    return $ ConnectionEvent timestamp client Connected

-- | Main parser for a 'client disconnected' log line.
--   Returns the parsed ConnectionEvent.
--   Expects format: timestamp|INFO|...|client disconnected 'Name'(id:XXX) reason 'reasonmsg=...'
clientDisconnectedParser :: Parser ConnectionEvent
clientDisconnectedParser = do
    timestamp <- timestampParser
    _ <- string "INFO    |VirtualServerBase|"
    _ <- takeWhile1P (Just "server id") (/= ' ')
    _ <- string "  |"
    _ <- string "client disconnected "
    client <- clientInfoParser
    _ <- takeWhileP (Just "trailing") (/= '\n')
    return $ ConnectionEvent timestamp client Disconnected

-- | Attempts to parse either a connected or disconnected event on a single line.
--   This parser tries both and returns the first successful result.
connectionEventParser :: Parser ConnectionEvent
connectionEventParser = try clientConnectedParser <|> try clientDisconnectedParser