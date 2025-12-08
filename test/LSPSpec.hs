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
      decoded `shouldBe` Success (fileURIFromStr "file://test")

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
    it "extracts the path from an URI correctly" $ do
      let uri = fileURIFromStr "file:///foo/bar.txt"
      pathFromURI uri `shouldBe` os "/foo/bar.txt"
