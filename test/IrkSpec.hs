module IrkSpec (spec) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Irk
import Language (languages)
import Test.Hspec
import Utils (FileKind (..), FilePathKind (..), os)

spec :: Spec
spec = do
  describe "searchPaths" $ do
    let py = fromJust $ Map.lookup "python" languages

    it "retrieves paths correctly (case 1)" $ do
      let searches = searchPaths py Nothing [os "test/data/python"]
      allPaths <- sequence searches
      allPaths
        `shouldBe` [ [],
                     [],
                     [],
                     [FilePathKind (os "test/data/python/test.py") Workspace],
                     [FilePathKind (os "test/data/python/.venv/vendored.py") WorkspaceVendored],
                     []
                   ]

    it "retrieves paths correctly (case 2)" $ do
      let searches = searchPaths py (Just (os "test/data/python/foo.py")) [os "test/data/python"]
      allPaths <- sequence searches
      allPaths
        `shouldBe` [ [FilePathKind (os "test/data/python/foo.py") Current],
                     [FilePathKind (os "test/data/python/test.py") Workspace],
                     [FilePathKind (os "test/data/python/.venv/vendored.py") WorkspaceVendored],
                     [],
                     [],
                     []
                   ]
