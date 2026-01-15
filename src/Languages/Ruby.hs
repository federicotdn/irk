module Languages.Ruby
  ( extensions,
    searchPath,
    symbolAtPosition,
    findSymbolDefinition,
  )
where

import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Ignore (Ignore, parse)
import Languages.Common
  ( Parser,
    baseIgnore,
    recurseDirectory,
    searchForMatch,
    symbolAtPos,
    vchar,
    vhspace,
    vhspace1,
    vspace1,
    vstring,
  )
import System.OsPath (OsString)
import Text.Megaparsec (SourcePos, choice, getSourcePos, try, (<|>))
import Types (IrkFile (..), IrkFileArea (..), IrkFilePos (..))
import Utils (oss)

extensions :: [OsString]
extensions = oss [".rb"]

ignore :: Ignore
ignore =
  baseIgnore
    <> parse
      ( T.unlines
          [ "/vendor/",
            "!*.rb"
          ]
      )

ignoreForVendor :: Ignore
ignoreForVendor =
  baseIgnore
    <> parse
      ( T.unlines
          [ "/*/",
            "!/vendor/",
            "!*.rb",
            "/*.rb"
          ]
      )

searchPath :: IrkFile -> IO [IrkFile]
searchPath origin = do
  case iArea origin of
    Workspace -> recurseDirectory ignore origin
    WorkspaceVendored -> recurseDirectory ignoreForVendor origin
    External -> return []

symbolAtPosition :: Text -> IrkFilePos -> Maybe Text
symbolAtPosition = symbolAtPos isIdentifierChar isIdentifier

isIdentifierChar :: Char -> Bool
isIdentifierChar ch = isAlphaNum ch || ch == '_'

isIdentifier :: Text -> Bool
isIdentifier i = maybe False (not . isDigit . fst) $ T.uncons i

findSymbolDefinition :: Text -> Text -> [IrkFilePos]
findSymbolDefinition symbol =
  searchForMatch $
    choice [try (findMethodDef symbol), try (findClassDef symbol), findModuleDef symbol]

findMethodDef :: Text -> Parser SourcePos
findMethodDef name = do
  vhspace
  vstring "def"
  vhspace1
  pos <- getSourcePos
  vstring name
  vchar '(' <|> vchar '\n'
  return pos

findClassDef :: Text -> Parser SourcePos
findClassDef name = do
  vhspace
  vstring "class"
  vhspace1
  pos <- getSourcePos
  vstring name
  vspace1
  return pos

findModuleDef :: Text -> Parser SourcePos
findModuleDef name = do
  vhspace
  vstring "module"
  vhspace1
  pos <- getSourcePos
  vstring name
  vspace1
  return pos
