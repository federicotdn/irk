module Testing (readTestFile, readTextFile) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import System.FilePath ((</>))
import Utils (fileText, os)
import Types (IrkFile(..), emptyFile)

readTextFile :: FilePath -> IO Text
readTextFile path = do
  -- Using Just 1 will force the fileText function to read the file.
  let f = emptyFile { iPath = os path, iFileSize = Just 1 }
  contents <- fileText f
  return $ fromJust contents

readTestFile :: FilePath -> IO Text
readTestFile path = readTextFile ("test" </> "data" </> path)
