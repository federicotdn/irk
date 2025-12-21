module Testing (success, readTestFile, readTextFile) where

import Data.Aeson.Types (Result (..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import System.FilePath ((</>))
import Types (IrkFile (..), file)
import Utils (fileText, os)

success :: Result a -> a
success (Error _) = error "expected Success, but got Error"
success (Success x) = x

readTextFile :: FilePath -> IO Text
readTextFile path = do
  let f = file {iPath = os path}
  contents <- fileText f
  return $ fromJust contents

readTestFile :: FilePath -> IO Text
readTestFile path = readTextFile ("test" </> "data" </> path)
