module UtilsSpec (spec) where

import qualified System.Info as I
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Control.Monad (when)
import Testing (readTextFile)
import Types (IrkFilePos (..), file)
import Utils

spec :: Spec
spec = do
  describe "extractLine" $ do
    it "extracts a line correctly" $ do
      extractLine "" (IrkFilePos file 0 0) `shouldBe` Nothing
      extractLine "hello" (IrkFilePos file 0 0) `shouldBe` Just ("", "hello")
      extractLine "hello" (IrkFilePos file 0 3) `shouldBe` Just ("hel", "lo")
      extractLine "hello" (IrkFilePos file 0 5) `shouldBe` Just ("hello", "")
      extractLine "hello" (IrkFilePos file 1 0) `shouldBe` Nothing
      extractLine "hello" (IrkFilePos file 1 10) `shouldBe` Nothing

  describe "fileText" $ do
    it "handles valid UTF-8 bytes" $ do
      tmpDir <- getTemporaryDirectory
      let testFile = tmpDir </> "utf8-test.txt"
      writeFile testFile "mañana"
      result <- readTextFile testFile
      result `shouldBe` "mañana"

  describe "longestPrefix" $ do
    it "calculates the longest prefix correctly" $ do
      longestPrefix (os "foo/bar/baz") (oss ["foo", "foo/bar"]) `shouldBe` Just (os "foo/bar")
      longestPrefix (os "foo/bar/baz") (oss ["foo/bar", "foo/bar"]) `shouldBe` Just (os "foo/bar")
      longestPrefix (os "foo/bar/baz") (oss ["ffoo", "ffoo/bar"]) `shouldBe` Nothing

  describe "hasWindowsDrive" $ do
    it "checks if a Windows drive is present" $ do
      hasWindowsDrive (os "") `shouldBe` False
      hasWindowsDrive (os "/foo") `shouldBe` False
      hasWindowsDrive (os "//foo") `shouldBe` False
      when (I.os == "mingw32") $ do
        hasWindowsDrive (os "C:\\foo") `shouldBe` True
        hasWindowsDrive (os "c:\\foo") `shouldBe` True
        hasWindowsDrive (os "c:\\foo\\bar") `shouldBe` True
