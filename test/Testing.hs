module Testing (success, readTestTextFile, getTestTextFileSize, readTextFile, asPosix, asNative) where

import Data.Aeson.Types (Result (..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import System.FilePath ((</>))
import System.Directory.OsPath (getFileSize)
import System.OsPath (OsPath, pathSeparator)
import qualified System.OsString as OS
import Types (IrkFile (..), file)
import Utils (fileText, os, osc)

success :: Result a -> a
success (Error _) = error "expected Success, but got Error"
success (Success x) = x

readTextFile :: FilePath -> IO Text
readTextFile path = do
  let f = file {iPath = os path}
  contents <- fileText f
  return $ fromJust contents

readTestTextFile :: FilePath -> IO Text
readTestTextFile path = readTextFile ("test" </> "data" </> path)

getTestTextFileSize :: FilePath -> IO Integer
getTestTextFileSize path = getFileSize $ os ("test" </> "data" </> path)

asPosix :: OsPath -> OsPath
asPosix path = OS.map (\c -> if c == osc '\\' then osc '/' else c) path

asNative :: OsPath -> OsPath
asNative path = OS.map (\c -> if c == osc '/' then pathSeparator else c) path
