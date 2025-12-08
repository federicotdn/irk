module Languages.Haskell (extensions, searchPaths) where

import System.OsPath (OsString)
import Utils (FilePathKind, Search, os)

extensions :: [OsString]
extensions = [os ".hs"]

searchPaths :: Search -> IO [FilePathKind]
searchPaths = undefined
