module Languages.Go
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
    vsatisfy,
    vskipWhile1,
    vstring,
  )
import System.OsPath (OsString)
import Text.Megaparsec (SourcePos, choice, getSourcePos)
import Types (IrkFile (..), IrkFileArea (..), IrkFilePos (..))
import Utils (os)

extensions :: [OsString]
extensions = [os ".go"]

ignore :: Ignore
ignore =
  baseIgnore
    <> parse
      ( T.unlines
          [ "/vendor/",
            "!*.go"
          ]
      )

ignoreForVendor :: Ignore
ignoreForVendor =
  baseIgnore
    <> parse
      ( T.unlines
          [ "/*/",
            "!/vendor/",
            "internal/",
            "!*.go",
            "/*.go"
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

-- | Validate an identifier, with the assumption that all characters follow 'isIdentifierChar'.
isIdentifier :: Text -> Bool
isIdentifier i = maybe False (not . isDigit . fst) $ T.uncons i

findSymbolDefinition :: Text -> Text -> [IrkFilePos]
findSymbolDefinition symbol = searchForMatch $ choice [findFuncDef symbol, findTypeDef symbol]

findTypeDef :: Text -> Parser SourcePos
findTypeDef name = do
  vstring "type"
  vhspace1
  pos <- getSourcePos
  vstring name
  vsatisfy (`elem` [' ', '[', '='])
  return pos

findFuncDef :: Text -> Parser SourcePos
findFuncDef name = do
  -- There could be spaces before 'func' but this never
  -- happens in practice.
  vstring "func"
  vhspace1
  voptional $ do
    vchar '('
    vskipWhile1 (/= ')')
    vchar ')'
    vhspace1
  pos <- getSourcePos
  vstring name
  voptional $ do
    vchar '['
    vskipWhile1 (/= ']')
    vchar ']'
  vhspace
  vchar '('
  vskipWhile1 (/= '{')
  vchar '{'
  return pos
