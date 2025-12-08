module Testing (readTestFile) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import System.FilePath ((</>))
import Utils (fileText, os)

readTestFile :: FilePath -> IO Text
readTestFile path = do
  contents <- fileText $ os $ "test" </> "data" </> path
  return $ fromJust contents
