module UtilsSpec (spec) where

import qualified Data.ByteString as BS
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "extractLine" $ do
    it "extracts a line correctly" $ do
      extractLine "" (FilePos Nothing 0 0) `shouldBe` Nothing
      extractLine "hello" (FilePos Nothing 0 0) `shouldBe` Just ("", "hello")
      extractLine "hello" (FilePos Nothing 0 3) `shouldBe` Just ("hel", "lo")
      extractLine "hello" (FilePos Nothing 0 5) `shouldBe` Just ("hello", "")
      extractLine "hello" (FilePos Nothing 1 0) `shouldBe` Nothing
      extractLine "hello" (FilePos Nothing 1 10) `shouldBe` Nothing

  describe "fileText" $ do
    it "handles invalid UTF-8 bytes" $ do
      tmpDir <- getTemporaryDirectory
      let testFile = tmpDir </> "invalid-utf8-test.txt"
      BS.writeFile testFile (BS.pack [0xFF, 0xFE, 0x48, 0x69])
      result <- fileText $ os testFile
      result `shouldSatisfy` (/= Nothing)

    it "handles valid UTF-8 bytes" $ do
      tmpDir <- getTemporaryDirectory
      let testFile = tmpDir </> "utf8-test.txt"
      writeFile testFile "mañana"
      result <- fileText $ os testFile
      result `shouldBe` Just "mañana"

  describe "longestPrefix" $ do
    it "calculates the longest prefix correctly" $ do
      longestPrefix (os "foo/bar/baz") (oss ["foo", "foo/bar"]) `shouldBe` Just (os "foo/bar")
      longestPrefix (os "foo/bar/baz") (oss ["foo/bar", "foo/bar"]) `shouldBe` Just (os "foo/bar")
      longestPrefix (os "foo/bar/baz") (oss ["ffoo", "ffoo/bar"]) `shouldBe` Nothing
