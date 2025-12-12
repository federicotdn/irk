module Language (Language (..), languageFor, languages) where

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Languages.C as LangC
import qualified Languages.Go as LangGo
import qualified Languages.Haskell as LangHs
import qualified Languages.Python as LangPy
import System.OsPath (OsPath, OsString, takeExtension)
import Utils (FilePathKind, FilePos, Search)

data Language = Language
  { lName :: String,
    lExtensions :: [OsString],
    lSearchPaths :: Search -> IO [FilePathKind],
    lSymbolAtPosition :: Text -> FilePos -> Maybe Text,
    lFindSymbolDefinition :: Text -> Text -> [FilePos]
  }

instance Show Language where
  show l = "<Language: " ++ lName l ++ ">"

languages :: Map String Language
languages =
  Map.fromList
    [ ( "haskell",
        Language
          { lName = "Haskell",
            lExtensions = LangHs.extensions,
            lSearchPaths = LangHs.searchPaths,
            lSymbolAtPosition = LangHs.symbolAtPosition,
            lFindSymbolDefinition = LangHs.findSymbolDefinition
          }
      ),
      ( "python",
        Language
          { lName = "Python",
            lExtensions = LangPy.extensions,
            lSearchPaths = LangPy.searchPaths,
            lSymbolAtPosition = LangPy.symbolAtPosition,
            lFindSymbolDefinition = LangPy.findSymbolDefinition
          }
      ),
      ( "go",
        Language
          { lName = "Go",
            lExtensions = LangGo.extensions,
            lSearchPaths = LangGo.searchPaths,
            lSymbolAtPosition = LangGo.symbolAtPosition,
            lFindSymbolDefinition = LangGo.findSymbolDefinition
          }
      ),
      ( "c",
        Language
          { lName = "C",
            lExtensions = LangC.extensions,
            lSearchPaths = LangC.searchPaths,
            lSymbolAtPosition = LangC.symbolAtPosition,
            lFindSymbolDefinition = LangC.findSymbolDefinition
          }
      )
    ]

languageFor :: OsPath -> Maybe Language
languageFor path = find (elem (takeExtension path) . lExtensions) $ Map.elems languages
