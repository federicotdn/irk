module Languages.Haskell
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
    vskipWhile,
    vskipWhile1,
    vstring,
  )
import System.OsPath (OsString)
import Text.Megaparsec (SourcePos, choice, getSourcePos, notFollowedBy, try, (<|>))
import Types (IrkFile (..), IrkFileArea (..), IrkFilePos (..))
import Utils (oss)

extensions :: [OsString]
extensions = oss [".hs", ".hsc"]

ignore :: Ignore
ignore =
  baseIgnore
    <> parse
      ( T.unlines
          [ "_build/",
            "dist-newstyle/",
            "/vendor/",
            "!*.hs",
            "!*.hsc"
          ]
      )

ignoreForVendor :: Ignore
ignoreForVendor =
  baseIgnore
    <> parse
      ( T.unlines
          [ "/*/",
            "!/vendor/",
            "!*.hs",
            "!*.hsc"
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
isIdentifierChar ch = isAlphaNum ch || (ch `elem` ['\'', '_'])

-- | Validate an identifier, with the assumption that all characters follow 'isIdentifierChar'.
isIdentifier :: Text -> Bool
isIdentifier i = maybe False (not . isDigit . fst) $ T.uncons i

findSymbolDefinition :: Text -> Text -> [IrkFilePos]
findSymbolDefinition symbol =
  searchForMatch $
    choice
      [ findTypeDef symbol,
        try $ findCommentTypeDef symbol,
        findDef symbol,
        try $ findClassDef symbol,
        findClassConstrainedDef symbol,
        findModuleDef symbol,
        findCPPMacroDef symbol
      ]

findDef :: Text -> Parser SourcePos
findDef name = do
  vhspace
  pos <- getSourcePos
  vstring name
  vhspace
  vstring "::"
  return pos

findTypeDef :: Text -> Parser SourcePos
findTypeDef name = do
  vstring "data" <|> vstring "newtype" <|> vstring "type"
  vhspace1
  pos <- getSourcePos
  vstring name
  vhspace1 <|> vchar '='
  return pos

findClassDef :: Text -> Parser SourcePos
findClassDef name = do
  vstring "class"
  vhspace1
  pos <- getSourcePos
  vstring name
  vhspace1
  vskipWhile1 (`notElem` ['\n', '='])
  notFollowedBy $ vstring "=>"
  return pos

findClassConstrainedDef :: Text -> Parser SourcePos
findClassConstrainedDef name = do
  vstring "class"
  vhspace1
  vskipWhile1 (`notElem` ['\n', '='])
  vstring "=>"
  vhspace
  pos <- getSourcePos
  vstring name
  vhspace1
  return pos

findModuleDef :: Text -> Parser SourcePos
findModuleDef name = do
  vstring "module"
  vhspace1
  pos <- getSourcePos
  vstring name
  vhspace1 <|> vchar '('
  return pos

findCPPMacroDef :: Text -> Parser SourcePos
findCPPMacroDef name = do
  vstring "#define"
  vhspace1
  pos <- getSourcePos
  vstring name
  vhspace1 <|> vchar '('
  return pos

findCommentTypeDef :: Text -> Parser SourcePos
findCommentTypeDef name = do
  vskipWhile (/= '@')
  vstring "@type"
  vhspace
  pos <- getSourcePos
  vstring name
  vhspace
  vchar '='
  return pos
