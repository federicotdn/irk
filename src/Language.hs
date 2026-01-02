module Language
  ( Language (..),
    languageByPath,
    languageByName,
    languages,
  )
where

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Languages.C as LangC
import qualified Languages.Go as LangGo
import qualified Languages.Haskell as LangHs
import qualified Languages.Python as LangPy
import System.OsPath (OsPath, OsString, takeExtension)
import Types (IrkFile, IrkFilePos)

data Language = Language
  { lName :: String,
    lExtensions :: [OsString],
    lSearchPath :: IrkFile -> IO [IrkFile],
    lSymbolAtPosition :: Text -> IrkFilePos -> Maybe Text,
    lFindSymbolDefinition :: Text -> Text -> [IrkFilePos]
  }

languages :: Map String Language
languages =
  Map.fromList
    [ ( "haskell",
        Language
          { lName = "Haskell",
            lExtensions = LangHs.extensions,
            lSearchPath = LangHs.searchPath,
            lSymbolAtPosition = LangHs.symbolAtPosition,
            lFindSymbolDefinition = LangHs.findSymbolDefinition
          }
      ),
      ( "python",
        Language
          { lName = "Python",
            lExtensions = LangPy.extensions,
            lSearchPath = LangPy.searchPath,
            lSymbolAtPosition = LangPy.symbolAtPosition,
            lFindSymbolDefinition = LangPy.findSymbolDefinition
          }
      ),
      ( "go",
        Language
          { lName = "Go",
            lExtensions = LangGo.extensions,
            lSearchPath = LangGo.searchPath,
            lSymbolAtPosition = LangGo.symbolAtPosition,
            lFindSymbolDefinition = LangGo.findSymbolDefinition
          }
      ),
      ( "c",
        Language
          { lName = "C",
            lExtensions = LangC.extensions,
            lSearchPath = LangC.searchPath,
            lSymbolAtPosition = LangC.symbolAtPosition,
            lFindSymbolDefinition = LangC.findSymbolDefinition
          }
      )
    ]

languageByPath :: OsPath -> Maybe Language
languageByPath path = find (elem (takeExtension path) . lExtensions) $ Map.elems languages

languageByName :: String -> Maybe Language
languageByName name = snd <$> find (\(k, v) -> k == name || lName v == name) (Map.toList languages)
