-- File: src/TeamSpeak/Parser/Internal.hs

-- | Internal module containing the low-level Megaparsec parsers for
--   TeamSpeak log events. Exposed to the main Parser module.
module TeamSpeak.Parser.Internal where

import           Control.Monad.Combinators (skipManyTill)
import           Data.Text (Text, pack)
import qualified Data.Text as T
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
    timestamp <- timestampParser
    -- We can skip the INFO part and other separators until we reach the keyword.
    -- This is a bit flexible but works for the given format.
    skipManyTill anySingle (try clientConnectedKeywordParser) -- Use 'try' to backtrack if 'client connected' doesn't match
    client <- clientInfoParser
    -- We can optionally parse the rest of the line (e.g., "using a myTeamSpeak ID...") if needed later,
    -- but for now, we just consume up to the end of the relevant part.
    -- Using 'takeRest' here might be too broad, let's just ensure we are at the end of the client info part.
    -- A more robust way might be to look for the end of the line or specific trailing patterns,
    -- but for initial parsing, stopping after the client info is sufficient if the format is consistent.
    -- For now, we assume the client info is immediately followed by the rest which we don't care about yet.
    -- So, just consume the client part and return.
    -- Let's consume the rest of the line for now, assuming the client info is the crucial part.
    -- takeRest -- This consumes everything after client info, which is fine for now.
    -- Actually, let's just ensure we parsed the client info correctly and stop there for the core event.
    -- Consume remaining characters on the line, effectively ignoring the rest (like IP:port)
    -- until we hit a newline or end of input. This makes the parser less brittle to format variations.
    -- However, 'anySingle' might not be ideal for the end. Let's use 'takeWhileP' for the rest.
    -- Let's just stop parsing after the client info. The rest is context but not part of the core event data.
    -- So, we just return the event with the timestamp, client, and type.
    return $ ConnectionEvent timestamp client Connected

-- | Main parser for a 'client disconnected' log line.
--   Returns the parsed ConnectionEvent.
--   Expects format: timestamp|INFO|...|client disconnected 'Name'(id:XXX) reason 'reasonmsg=...'
clientDisconnectedParser :: Parser ConnectionEvent
clientDisconnectedParser = do
    timestamp <- timestampParser
    skipManyTill anySingle (try clientDisconnectedKeywordParser) -- Use 'try' to backtrack
    client <- clientInfoParser
    -- Optionally parse the reason part if needed later, e.g., 'reason 'reasonmsg=...'
    -- For now, just consume the rest of the line after client info.
    -- Similar to connected, we stop after parsing the client info.
    return $ ConnectionEvent timestamp client Disconnected

-- | Attempts to parse either a connected or disconnected event on a single line.
--   This parser tries both and returns the first successful result.
connectionEventParser :: Parser ConnectionEvent
connectionEventParser = try clientConnectedParser <|> try clientDisconnectedParser