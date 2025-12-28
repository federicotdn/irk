module IrkSpec (spec) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Irk
import Language (languages)
import Test.Hspec
import Testing (asPosix, getTestTextFileSize)
import Types (IrkFile (..), IrkFileArea (..))
import Utils (os)

-- Helper to normalize paths in results for cross-platform comparison
normalizePaths :: [[IrkFile]] -> [[IrkFile]]
normalizePaths = map (map (\f -> f {iPath = asPosix (iPath f)}))

spec :: Spec
spec = do
  describe "searchPaths" $ do
    let py = fromJust $ Map.lookup "python" languages

    it "retrieves paths correctly (case 1)" $ do
      testPySize <- Just <$> getTestTextFileSize "python/test.py"
      vendoredPySize <- Just <$> getTestTextFileSize "python/.venv/vendored.py"

      let searches = searchPaths py Nothing [os "test/data/python"]
      allPaths <- sequence searches
      normalizePaths allPaths
        `shouldBe` [ [],
                     [IrkFile (os "test/data/python/test.py") False testPySize 1 Workspace],
                     [IrkFile (os "test/data/python/.venv/vendored.py") False vendoredPySize 2 WorkspaceVendored],
                     []
                   ]

    it "retrieves paths correctly (case 2)" $ do
      testPySize <- Just <$> getTestTextFileSize "python/test.py"
      vendoredPySize <- Just <$> getTestTextFileSize "python/.venv/vendored.py"

      let searches = searchPaths py (Just (os "test/data/python/foo.py")) [os "test/data/python"]
      allPaths <- sequence searches
      normalizePaths allPaths
        `shouldBe` [ [IrkFile (os "test/data/python/foo.py") False Nothing 0 Workspace],
                     [IrkFile (os "test/data/python/test.py") False testPySize 1 Workspace],
                     [IrkFile (os "test/data/python/.venv/vendored.py") False vendoredPySize 2 WorkspaceVendored],
                     []
                   ]
