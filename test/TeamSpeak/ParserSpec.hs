-- test/TeamSpeak/ParserSpec.hs
module TeamSpeak.ParserSpec (spec) where

import Test.Hspec
import TeamSpeak.Parser (parseLogLine)

spec :: Spec
spec = describe "TeamSpeak log parser" $ do
  it "parses client connected event" $ do
    let line = "2024-12-24 10:35:22.987654|INFO    |VirtualServerBase|1  |client 'Alice'(id:123) connected to channel 'Lobby'(id:1)"
    case parseLogLine line of
      Just event -> do
        event `shouldBe` undefined  -- TODO: 构造期望值
      Nothing -> expectationFailure "Failed to parse"