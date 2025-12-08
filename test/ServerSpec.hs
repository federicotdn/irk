module ServerSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Value (String))
import LSP
import Server
import Test.Hspec

spec :: Spec
spec = do
  describe "handleMessage" $ do
    it "ignores notifications when the server is not initialized" $ do
      let srv = createServer False
      let notif = Notification {nJsonrpc = "2.0", nMethod = UnknownMethod "foo", nParams = Nothing}
      result <- handleMessage srv (MNotification notif)
      initializeDone result `shouldBe` False
      initializedDone result `shouldBe` False
      length (outbox result) `shouldBe` 0

    it "returns error when the server is not initialized and request is not initialize" $ do
      let srv = createServer False
      let req = Request {rJsonrpc = "2.0", rId = IDInt 1, rMethod = TextDocumentDefinition, rParams = Nothing}
      result <- handleMessage srv (MRequest req)
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
      result <- handleMessage srv (MRequest req)
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
      result <- handleMessage srv (MRequest req)
      shuttingDown result `shouldBe` True
