module Testing (readTestFile, readTextFile) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import System.FilePath ((</>))
import Types (IrkFile (..), file)
import Utils (fileText, os)

readTextFile :: FilePath -> IO Text
readTextFile path = do
  let f = file {iPath = os path}
  contents <- fileText f
  return $ fromJust contents

readTestFile :: FilePath -> IO Text
readTestFile path = readTextFile ("test" </> "data" </> path)
