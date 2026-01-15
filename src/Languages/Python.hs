module Languages.Python
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
    vstring,
  )
import System.OsPath (OsString)
import Text.Megaparsec
  ( SourcePos,
    choice,
    getSourcePos,
    try,
  )
import Types (IrkFile (..), IrkFileArea (..), IrkFilePos (..))
import Utils (oss)

extensions :: [OsString]
extensions = oss [".py", ".pyi"]

ignore :: Ignore
ignore =
  baseIgnore
    <> parse
      ( T.unlines
          [ "__pycache__",
            "/venv/",
            "/env/",
            "!*.py",
            "!*.pyi"
          ]
      )

ignoreForVendor :: Ignore
ignoreForVendor =
  baseIgnore
    <> parse
      ( T.unlines
          [ "__pycache__",
            "/*/",
            "!/.venv/",
            "!/.env/",
            "!/venv/",
            "!/env/",
            "lib64/",
            "!*.py",
            "!*.pyi",
            "/*.py"
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
findSymbolDefinition symbol =
  searchForMatch $
    choice
      [ findTopLevelAssignment symbol,
        -- Use 'try' here to avoid consuming initial whitespace in case we don't match.
        try (findFuncDef symbol),
        findClassDef symbol
      ]

findTopLevelAssignment :: Text -> Parser SourcePos
findTopLevelAssignment name = do
  pos <- getSourcePos
  vstring name
  vhspace
  vchar '='
  vhspace
  return pos

findClassDef :: Text -> Parser SourcePos
findClassDef name = do
  vhspace
  vstring "class"
  vhspace1
  pos <- getSourcePos
  vstring name
  vhspace
  voptional $ do
    vchar '('
    vskipWhile (/= ')')
    vchar ')'
    vhspace
  vchar ':'
  return pos

findFuncDef :: Text -> Parser SourcePos
findFuncDef name = do
  vhspace
  vstring "def"
  vhspace1
  pos <- getSourcePos
  vstring name
  vhspace
  vchar '('
  vskipWhile1 (/= ':')
  vchar ':'
  return pos
