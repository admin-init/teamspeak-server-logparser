-- src/TeamSpeak/Types.hs
module TeamSpeak.Types where

import Data.Time (UTCTime)

data EventType
  = ClientConnected { clientId :: Int, clientName :: String, channelId :: Int, channelName :: String }
  | ClientDisconnected { clientId :: Int, clientName :: String }
  | ChatMessage { senderId :: Int, senderName :: String, messageText :: String }
  deriving (Show, Eq)

data LogEvent = LogEvent
  { eventTime :: UTCTime
  , eventType :: EventType
  } deriving (Show, Eq)
