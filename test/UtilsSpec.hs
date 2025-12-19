module UtilsSpec (spec) where

import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Types (IrkFilePos (..), emptyFile)
import Utils
import Testing (readTextFile)

spec :: Spec
spec = do
  describe "extractLine" $ do
    it "extracts a line correctly" $ do
      extractLine "" (IrkFilePos emptyFile 0 0) `shouldBe` Nothing
      extractLine "hello" (IrkFilePos emptyFile 0 0) `shouldBe` Just ("", "hello")
      extractLine "hello" (IrkFilePos emptyFile 0 3) `shouldBe` Just ("hel", "lo")
      extractLine "hello" (IrkFilePos emptyFile 0 5) `shouldBe` Just ("hello", "")
      extractLine "hello" (IrkFilePos emptyFile 1 0) `shouldBe` Nothing
      extractLine "hello" (IrkFilePos emptyFile 1 10) `shouldBe` Nothing

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
