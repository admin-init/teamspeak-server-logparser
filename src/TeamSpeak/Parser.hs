-- File: src/TeamSpeak/Parser.hs

-- | Main module for parsing TeamSpeak log lines.
--   Exposes high-level parsing functions.
module TeamSpeak.Parser
    ( clientConnectedParser -- Export the connected parser
    , clientDisconnectedParser -- Export the disconnected parser (useful for future issues)
    , connectionEventParser -- Export the combined parser (useful for parsing a line generically)
    ) where

-- Import the internal module to access its parsers.
import TeamSpeak.Parser.Internal (clientConnectedParser, clientDisconnectedParser, connectionEventParser)