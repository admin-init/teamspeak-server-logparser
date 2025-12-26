-- File: src/TeamSpeak/Types.hs

-- | Module containing the core data types for representing TeamSpeak log events
--   and client information.
module TeamSpeak.Types
    ( Client(..)
    , ConnectionEvent(..)
    , ConnectionEventType(..)
    , UserSession(..)
    ) where

import Data.Time (UTCTime)
import Data.Text (Text)

-- | Represents a TeamSpeak client with its unique identifier and name.
data Client = Client
    { clientId   :: Int   -- ^ The unique numerical ID assigned to the client.
    , clientName :: Text  -- ^ The display name of the client.
    } deriving (Show, Eq) -- Added Eq for potential comparisons

-- | Represents a client connection or disconnection event parsed from the log.
data ConnectionEvent = ConnectionEvent
    { eventTimestamp :: UTCTime               -- ^ The time the event occurred.
    , eventClient    :: Client                -- ^ The client involved in the event.
    , eventType      :: ConnectionEventType   -- ^ Whether the event was a connection or disconnection.
    -- Potentially add reason :: Maybe Text later for disconnection reasons
    } deriving (Show, Eq)

-- | Distinguishes between a client connecting and disconnecting.
data ConnectionEventType
    = Connected    -- ^ Client connected to the server.
    | Disconnected -- ^ Client disconnected from the server.
    -- Potentially add other types later if needed (e.g., TimedOut)
    deriving (Show, Eq)

-- Using UTCTime for consistency with ConnectionEvent, assuming conversion for DB storage/retrieval
data UserSession = UserSession
    { sessionId :: Int
    , sessionClientId :: Int
    , sessionClientName :: Text
    , sessionConnectTime :: UTCTime -- Store as UTCTime, convert to/from ISO 8601 string for DB
    , sessionDisconnectTime :: Maybe UTCTime -- Store as Maybe UTCTime, convert to/from ISO 8601 string for DB (NULL if Nothing)
    } deriving (Show, Eq)