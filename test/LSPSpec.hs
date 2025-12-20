module LSPSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty')
import Data.Aeson.Types (Result (..), ToJSON, Value (Null), emptyArray, emptyObject, parse, parseJSON)
import qualified Data.ByteString.Lazy.Char8 as BL
import LSP
import Test.Hspec
import Utils (os)

assertEncoded :: (ToJSON a) => a -> String -> Expectation
assertEncoded msg expected =
  BL.unpack (encodePretty' config msg) `shouldBe` expected
  where
    config = defConfig {confCompare = compare}

fileURIFromStr :: String -> URI
fileURIFromStr s = FileURI $ os s

spec :: Spec
spec = do
  describe "parseJSON URI" $ do
    it "parses a valid file URI correctly" $ do
      let decoded = parse parseJSON "file://test" :: Result URI
      decoded `shouldBe` Success (fileURIFromStr "test")

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
    it "handles absolute Windows URIs with URL-encoded colons" $ do
      let decoded = parse parseJSON "file:///c%3A/Users/test/file.hs" :: Result URI
      pathFromURI <$> decoded `shouldBe` Success (os "c:/Users/test/file.hs")

  describe "uriFromPath" $ do
    it "converts absolute Windows paths to URIs correctly" $ do
      let path = os "c:/Users/test/file.hs"
      let uri = uriFromPath path
      uri `shouldBe` fileURIFromStr "/c:/Users/test/file.hs"
