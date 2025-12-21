module LSPSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty')
import Data.Aeson.Types (Result (..), ToJSON, Value (Null), emptyArray, emptyObject, parse, parseJSON)
import qualified Data.ByteString.Lazy.Char8 as BL
import LSP
import Test.Hspec
import Testing (success)
import Utils (os, tryEncoding)

assertEncoded :: (ToJSON a) => a -> String -> Expectation
assertEncoded msg expected =
  BL.unpack (encodePretty' config msg) `shouldBe` expected
  where
    config = defConfig {confCompare = compare}

spec :: Spec
spec = do
  describe "parseJSON URI" $ do
    it "parses a valid file URI correctly" $ do
      let decoded = success $ parse parseJSON "file://test" :: URI
      decoded `shouldBe` FileURI "test"

    it "errors out when parsing an invalid file URI" $ do
      let decoded = parse parseJSON "foo" :: Result URI
      decoded `shouldBe` Error "malformed URI"

  describe "toEncoding Request" $ do
    it "encodes a simple request correctly (no params)" $ do
      let req = Request {rJsonrpc = "2.0", rId = IDInt 1, rMethod = Initialize, rParams = Nothing}
      let expected =
            "{\n\
            \    \"id\": 1,\n\
            \    \"jsonrpc\": \"2.0\",\n\
            \    \"method\": \"initialize\"\n\
            \}"
      assertEncoded (MRequest req) expected

    it "encodes a simple request correctly (with params)" $ do
      let req =
            Request
              { rJsonrpc = "2.0",
                rId = IDInt 1,
                rMethod = Initialize,
                rParams =
                  Just $
                    object
                      [ "foo" .= ("bar" :: String),
                        "bar" .= emptyObject,
                        "baz" .= Null,
                        "quux" .= emptyArray
                      ]
              }
      let expected =
            "{\n\
            \    \"id\": 1,\n\
            \    \"jsonrpc\": \"2.0\",\n\
            \    \"method\": \"initialize\",\n\
            \    \"params\": {\n\
            \        \"bar\": {},\n\
            \        \"baz\": null,\n\
            \        \"foo\": \"bar\",\n\
            \        \"quux\": []\n\
            \    }\n\
            \}"
      assertEncoded (MRequest req) expected

  describe "toEncoding Response" $ do
    it "encodes a simple response correctly" $ do
      let req = Response {pJsonrpc = "2.0", pId = Just $ IDString "x", pResult = Just emptyObject, pError = Nothing}
      let expected =
            "{\n\
            \    \"id\": \"x\",\n\
            \    \"jsonrpc\": \"2.0\",\n\
            \    \"result\": {}\n\
            \}"
      assertEncoded (MResponse req) expected

  describe "pathFromURI" $ do
    it "handles absolute URIs for Unix" $ do
      let decoded = success $ parse parseJSON "file:///home/user/test.hs" :: URI
      result <- tryEncoding $ pathFromURI decoded
      result `shouldBe` Just (os "/home/user/test.hs")

    it "handles absolute Windows URIs with URL-encoded colons" $ do
      -- NOTE: This test will not actually pass when run on Windows.
      let decoded = success $ parse parseJSON "file:///c%3A/Users/test/file.hs" :: URI
      result <- tryEncoding $ pathFromURI decoded
      result `shouldBe` Just (os "c:/Users/test/file.hs")

  describe "uriFromPath" $ do
    it "converts absolute Unix paths to URIs correctly" $ do
      let path = os "/home/test/foo.py"
      uri <- tryEncoding $ uriFromPath path
      uri `shouldBe` Just (FileURI "/home/test/foo.py")

    it "converts absolute Windows paths to URIs correctly" $ do
      let path = os "c:/Users/test/file.hs"
      uri <- tryEncoding $ uriFromPath path
      uri `shouldBe` Just (FileURI "/c:/Users/test/file.hs")
