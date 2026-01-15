module Languages.JavaScript
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
    vstring,
  )
import System.OsPath (OsString)
import Text.Megaparsec
  ( SourcePos,
    choice,
    getSourcePos,
    (<|>),
  )
import Types (IrkFile (..), IrkFileArea (..), IrkFilePos (..))
import Utils (oss)

extensions :: [OsString]
extensions = oss [".js", ".mjs", ".cjs"]

ignore :: Ignore
ignore =
  baseIgnore
    <> parse
      ( T.unlines
          [ "node_modules/",
            "dist/",
            "build/",
            "*.min.js",
            "*.bundle.js",
            "!*.js",
            "!*.mjs",
            "!*.cjs"
          ]
      )

ignoreForVendor :: Ignore
ignoreForVendor =
  baseIgnore
    <> parse
      ( T.unlines
          [ "/*/",
            "!/node_modules/",
            "*.min.js",
            "*.bundle.js",
            "!*.js",
            "!*.mjs",
            "!*.cjs",
            "/*.js",
            "/*.mjs",
            "/*.cjs"
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
isIdentifierChar ch = isAlphaNum ch || ch == '_' || ch == '$'

-- | Validate an identifier, with the assumption that all characters follow 'isIdentifierChar'.
isIdentifier :: Text -> Bool
isIdentifier i = maybe False (\(c, _) -> not (isDigit c)) $ T.uncons i

findSymbolDefinition :: Text -> Text -> [IrkFilePos]
findSymbolDefinition symbol =
  searchForMatch $
    choice
      [ findFuncDef symbol,
        findClassDef symbol
      ]

-- | Match: function name(...) {
findFuncDef :: Text -> Parser SourcePos
findFuncDef name = do
  vhspace
  voptional $ do
    vstring "export"
    vhspace1
  voptional $ do
    vstring "async"
    vhspace1
  vstring "function"
  vhspace
  voptional $ vchar '*' -- generator
  vhspace
  pos <- getSourcePos
  vstring name
  vhspace
  vchar '('
  return pos

-- | Match: class Name
findClassDef :: Text -> Parser SourcePos
findClassDef name = do
  vhspace
  voptional $ do
    vstring "export"
    vhspace1
  voptional $ do
    vstring "default"
    vhspace1
  vstring "class"
  vhspace1
  pos <- getSourcePos
  vstring name
  vhspace
  vchar '{' <|> vchar ' '
  return pos
