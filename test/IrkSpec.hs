module IrkSpec (spec) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Irk
import Language (languages)
import System.OsPath (OsPath)
import Test.Hspec
import Testing (asPosix)
import Types (IrkFile (..))
import Utils (os)

getPaths :: [[IrkFile]] -> [[OsPath]]
getPaths = map (map (asPosix . iPath))

spec :: Spec
spec = do
  describe "searchPaths" $ do
    let py = fromJust $ Map.lookup "python" languages
    let go = fromJust $ Map.lookup "go" languages

    it "retrieves paths correctly (case 1)" $ do
      let searches = searchPaths py Nothing [os "test/data/python"]
      allPaths <- sequence searches
      getPaths allPaths
        `shouldBe` [ [],
                     [os "test/data/python/test.py"],
                     [os "test/data/python/.venv/lib/vendored.py"],
                     []
                   ]

    it "retrieves paths correctly (case 2)" $ do
      let searches = searchPaths py (Just (os "test/data/python/foo.py")) [os "test/data/python"]
      allPaths <- sequence searches
      getPaths allPaths
        `shouldBe` [ [os "test/data/python/foo.py"],
                     [os "test/data/python/test.py"],
                     [os "test/data/python/.venv/lib/vendored.py"],
                     []
                   ]

    it "retrieves paths correctly (case 3)" $ do
      let searches = searchPaths go Nothing [os "test/data/go"]
      allPaths <- sequence searches
      getPaths allPaths
        `shouldBe` [ [],
                     [os "test/data/go/main.go"],
                     [os "test/data/go/vendor/vendored.go"],
                     []
                   ]
