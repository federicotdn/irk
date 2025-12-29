module ServerSpec (spec) where

import Control.Monad.Trans.State (runStateT)
import Data.Aeson (object, (.=))
import Data.Aeson.Types (Value (String))
import LSP
import Server
import Test.Hspec

runSrvAction :: App () -> Server -> IO Server
runSrvAction app srv = do
  (_, result) <- runStateT app srv
  return result

spec :: Spec
spec = do
  describe "handleMessage" $ do
    it "ignores notifications when the server is not initialized" $ do
      let srv = createServer False
      let notif = Notification {nJsonrpc = "2.0", nMethod = UnknownMethod "foo", nParams = Nothing}
      result <- runSrvAction (handleMessage (MNotification notif)) srv
      initializeDone result `shouldBe` False
      initializedDone result `shouldBe` False
      length (outbox result) `shouldBe` 0

    it "returns error when the server is not initialized and request is not initialize" $ do
      let srv = createServer False
      let req = Request {rJsonrpc = "2.0", rId = IDInt 1, rMethod = TextDocumentDefinition, rParams = Nothing}
      result <- runSrvAction (handleMessage (MRequest req)) srv
      initializeDone result `shouldBe` False
      initializedDone result `shouldBe` False
      length (outbox result) `shouldBe` 1
      case head (outbox result) of
        MResponse Response {pError = Just err} ->
          eCode err `shouldBe` ServerNotInitialized
        _ -> expectationFailure "Expected error response"

    it "initializes (step 1) correctly" $ do
      let srv = createServer False
      let capabilities =
            object
              [ "general"
                  .= object
                    [ "positionEncodings" .= [UTF8]
                    ]
              ]
      let req =
            Request
              { rJsonrpc = "2.0",
                rId = IDInt 1,
                rMethod = Initialize,
                rParams =
                  Just $
                    object
                      [ "rootUri" .= String "file://test",
                        "capabilities" .= capabilities
                      ]
              }
      result <- runSrvAction (handleMessage (MRequest req)) srv
      initializeDone result `shouldBe` True
      positionEncoding result `shouldBe` UTF8

    it "commences shutdown correctly" $ do
      let srv = (createServer False) {initializeDone = True, initializedDone = True}
      let req =
            Request
              { rJsonrpc = "2.0",
                rId = IDInt 1,
                rMethod = Shutdown,
                rParams = Nothing
              }
      result <- runSrvAction (handleMessage (MRequest req)) srv
      shuttingDown result `shouldBe` True

  describe "positionToUTF32" $ do
    it "returns position as-is when encoding is already UTF32" $ do
      let pos = Position {pLine = 0, pCharacter = 9}
      positionToUTF32 pos UTF32 "helló world" `shouldBe` pos

    it "converts UTF8 position to UTF32 with multibyte characters" $ do
      let pos = Position {pLine = 0, pCharacter = 10}
      let result = positionToUTF32 pos UTF8 "hélló world"
      pCharacter result `shouldBe` 8

    it "converts UTF8 position to UTF32 with multibyte characters (w/ invalid col)" $ do
      let pos = Position {pLine = 0, pCharacter = 100}
      let result = positionToUTF32 pos UTF8 "hélló world"
      pCharacter result `shouldBe` 11

    it "converts UTF8 position to UTF32 with multibyte characters (w/ null string)" $ do
      let pos = Position {pLine = 0, pCharacter = 0}
      let result = positionToUTF32 pos UTF8 ""
      pCharacter result `shouldBe` 0

    it "converts UTF8 position to UTF16 with multibyte characters" $ do
      let pos = Position {pLine = 0, pCharacter = 6}
      let result = positionToUTF32 pos UTF16 "hélló world"
      pCharacter result `shouldBe` 6

    it "returns position as-is when line is not found" $ do
      let pos = Position {pLine = 10, pCharacter = 5}
      positionToUTF32 pos UTF8 "hello" `shouldBe` pos

  describe "positionFromUTF32" $ do
    it "round-trip UTF8 position remains unchanged" $ do
      let pos = Position {pLine = 0, pCharacter = 10}
      let text = "hélló world"
      let utf32Pos = positionToUTF32 pos UTF8 text
      positionFromUTF32 utf32Pos UTF8 text `shouldBe` pos

    it "round-trip UTF16 position remains unchanged" $ do
      let pos = Position {pLine = 0, pCharacter = 6}
      let text = "hélló world"
      let utf32Pos = positionToUTF32 pos UTF16 text
      positionFromUTF32 utf32Pos UTF16 text `shouldBe` pos

    it "round-trip UTF32 position remains unchanged" $ do
      let pos = Position {pLine = 0, pCharacter = 9}
      let text = "helló world"
      let utf32Pos = positionToUTF32 pos UTF32 text
      positionFromUTF32 utf32Pos UTF32 text `shouldBe` pos

    it "round-trip with invalid line returns original position" $ do
      let pos = Position {pLine = 10, pCharacter = 5}
      let text = "hello"
      let utf32Pos = positionToUTF32 pos UTF8 text
      positionFromUTF32 utf32Pos UTF8 text `shouldBe` pos
