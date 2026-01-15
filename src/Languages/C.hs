module Languages.C
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
    voptional,
    vskipWhile,
    vskipWhile1,
    vspace,
    vspace1,
    vstring,
  )
import System.OsPath (OsString)
import Text.Megaparsec (SourcePos, choice, getSourcePos, (<|>))
import Types (IrkFile (..), IrkFileArea (..), IrkFilePos (..))
import Utils (oss)

extensions :: [OsString]
extensions = oss [".c", ".h"]

ignore :: Ignore
ignore =
  baseIgnore
    <> parse
      ( T.unlines
          [ "!*.c",
            "!*.h"
          ]
      )

searchPath :: IrkFile -> IO [IrkFile]
searchPath origin = do
  case iArea origin of
    Workspace -> do
      recurseDirectory ignore origin
    WorkspaceVendored -> return []
    External -> return []

symbolAtPosition :: Text -> IrkFilePos -> Maybe Text
symbolAtPosition = symbolAtPos isIdentifierChar isIdentifier

isIdentifierChar :: Char -> Bool
isIdentifierChar ch = isAlphaNum ch || ch == '_'

-- | Validate an identifier, with the assumption that all characters follow 'isIdentifierChar'.
isIdentifier :: Text -> Bool
isIdentifier i = maybe False (not . isDigit . fst) $ T.uncons i

findSymbolDefinition :: Text -> Text -> [IrkFilePos]
findSymbolDefinition symbol = searchForMatch $ choice [findMacroDef symbol, findFuncDef symbol]

findMacroDef :: Text -> Parser SourcePos
findMacroDef name = do
  vstring "#define"
  vhspace1
  pos <- getSourcePos
  vstring name
  vhspace1 <|> vchar '('
  return pos

findFuncDef :: Text -> Parser SourcePos
findFuncDef name = do
  vhspace
  voptional $ do
    vstring "static"
    vspace1
    pure ()
  vskipWhile1 isIdentifierChar
  vspace1
  pos <- getSourcePos
  vstring name
  vspace
  vchar '('
  vskipWhile (/= ')')
  vchar ')'
  vspace
  vchar '{'
  return pos
