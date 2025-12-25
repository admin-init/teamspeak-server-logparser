-- File: test/TeamSpeak/ParserSpec.hs
{-# LANGUAGE OverloadedStrings #-}

module TeamSpeak.ParserSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse, errorBundlePretty, Parsec)
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Text (Text, pack)
import Data.Void (Void)
import TeamSpeak.Types (Client (..), ConnectionEvent (..), ConnectionEventType (..))
import TeamSpeak.Parser (clientConnectedParser, clientDisconnectedParser, connectionEventParser)

-- Helper function to run a parser and extract the result or fail with an error message
runParser :: Parsec Void Text a -> Text -> a
runParser p input =
  case parse p "" input of
    Left bundle -> error $ "Parse error:\n" ++ errorBundlePretty bundle
    Right result -> result

-- Helper to parse a specific time string to UTCTime
parseTestTime :: String -> UTCTime
parseTestTime s = case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" s of
                    Nothing -> error $ "Could not parse test time: " ++ s
                    Just t -> t

expectedTimeConnected :: UTCTime
expectedTimeConnected = parseTestTime "2025-08-21 03:02:02.318469"

expectedTimeDisconnected :: UTCTime
expectedTimeDisconnected = parseTestTime "2025-08-21 02:57:55.329933"

spec :: Spec
spec = do
  describe "clientConnectedParser" $ do
    it "parses a client connected log line correctly" $ do
      let logLine = "2025-08-21 03:02:02.318469|INFO    |VirtualServerBase|1  |client connected 'TEST_USER'(id:3) using a myTeamSpeak ID from 60.176.94.92:53623"
      let expectedEvent = ConnectionEvent
                              { eventTimestamp = expectedTimeConnected
                              , eventClient = Client { clientId = 3, clientName = "TEST_USER" }
                              , eventType = Connected
                              }
      runParser clientConnectedParser (pack logLine) `shouldBe` expectedEvent

  describe "clientDisconnectedParser" $ do
    it "parses a client disconnected log line correctly" $ do
      let logLine = "2025-08-21 02:57:55.329933|INFO    |VirtualServerBase|1  |client disconnected 'TEST_USER'(id:3) reason 'reasonmsg=leaving'"
      let expectedEvent = ConnectionEvent
                              { eventTimestamp = expectedTimeDisconnected
                              , eventClient = Client { clientId = 3, clientName = "TEST_USER" }
                              , eventType = Disconnected
                              }
      runParser clientDisconnectedParser (pack logLine) `shouldBe` expectedEvent

  describe "connectionEventParser" $ do
    it "parses a client connected line using the combined parser" $ do
      let logLine = "2025-08-21 03:02:02.318469|INFO    |VirtualServerBase|1  |client connected 'TEST_USER'(id:3) using a myTeamSpeak ID from 60.176.94.92:53623"
      let expectedEvent = ConnectionEvent
                              { eventTimestamp = expectedTimeConnected
                              , eventClient = Client { clientId = 3, clientName = "TEST_USER" }
                              , eventType = Connected
                              }
      runParser connectionEventParser (pack logLine) `shouldBe` expectedEvent

    it "parses a client disconnected line using the combined parser" $ do
      let logLine = "2025-08-21 02:57:55.329933|INFO    |VirtualServerBase|1  |client disconnected 'TEST_USER'(id:3) reason 'reasonmsg=leaving'"
      let expectedEvent = ConnectionEvent
                              { eventTimestamp = expectedTimeDisconnected
                              , eventClient = Client { clientId = 3, clientName = "TEST_USER" }
                              , eventType = Disconnected
                              }
      runParser connectionEventParser (pack logLine) `shouldBe` expectedEvent

    -- Example of a line that should fail the connectionEventParser (e.g., a different event type)
    it "fails to parse a non-connection/disconnection line" $ do
      let logLine = "2025-08-21 08:13:13.365893|INFO    |PktHandler    |1  |Dropping client 39 because of ping timeout 19 0 0"
      -- We expect this to fail. `parse` returns `Left` on failure.
      case parse connectionEventParser "" (pack logLine) of
        Left _ -> return () -- Test passes if it fails to parse
        Right _ -> expectationFailure "Expected parser to fail for a non-connection line, but it succeeded."