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
  )
import System.OsPath (OsString)
import Text.Megaparsec (SourcePos, choice, getSourcePos, try, (<|>))
import Text.Megaparsec.Char (hspace1, string, char, space1, hspace)
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
  _ <- hspace
  _ <- string "def"
  _ <- hspace1
  pos <- getSourcePos
  _ <- string name
  _ <- char '(' <|> char '\n'
  return pos

findClassDef :: Text -> Parser SourcePos
findClassDef name = do
  _ <- hspace
  _ <- string "class"
  _ <- hspace1
  pos <- getSourcePos
  _ <- string name
  _ <- space1
  return pos

findModuleDef :: Text -> Parser SourcePos
findModuleDef name = do
  _ <- hspace
  _ <- string "module"
  _ <- hspace1
  pos <- getSourcePos
  _ <- string name
  _ <- space1
  return pos
