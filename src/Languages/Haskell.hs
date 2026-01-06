module Languages.Haskell
  ( extensions,
    searchPath,
    symbolAtPosition,
    findSymbolDefinition,
  )
where

import Control.Monad (void)
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
import Text.Megaparsec (SourcePos, choice, getSourcePos, notFollowedBy, takeWhile1P, try, (<|>))
import Text.Megaparsec.Char (char, hspace, hspace1, string)
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
      [ findDef symbol,
        findTypeDef symbol,
        -- Use 'try' here to avoid consuming initial tokens in case we don't match.
        try $ findClassDef symbol,
        findClassConstrainedDef symbol,
        findModuleDef symbol,
        findCPPMacroDef symbol
      ]

findDef :: Text -> Parser SourcePos
findDef name = do
  pos <- getSourcePos
  _ <- string name
  _ <- hspace
  _ <- string "::"
  return pos

findTypeDef :: Text -> Parser SourcePos
findTypeDef name = do
  _ <- string "data" <|> string "newtype" <|> string "type"
  _ <- hspace1
  pos <- getSourcePos
  _ <- string name
  _ <- hspace1 <|> void (char '=')
  return pos

findClassDef :: Text -> Parser SourcePos
findClassDef name = do
  _ <- string "class"
  _ <- hspace1
  pos <- getSourcePos
  _ <- string name
  _ <- hspace1
  _ <- takeWhile1P Nothing (`notElem` ['\n', '='])
  notFollowedBy $ string "=>"
  return pos

findClassConstrainedDef :: Text -> Parser SourcePos
findClassConstrainedDef name = do
  _ <- string "class"
  _ <- hspace1
  _ <- takeWhile1P Nothing (`notElem` ['\n', '='])
  _ <- string "=>"
  _ <- hspace
  pos <- getSourcePos
  _ <- string name
  _ <- hspace1
  return pos

findModuleDef :: Text -> Parser SourcePos
findModuleDef name = do
  _ <- string "module"
  _ <- hspace1
  pos <- getSourcePos
  _ <- string name
  _ <- hspace1 <|> void (char '(')
  return pos

findCPPMacroDef :: Text -> Parser SourcePos
findCPPMacroDef name = do
  _ <- string "#define"
  _ <- hspace1
  pos <- getSourcePos
  _ <- string name
  _ <- hspace1 <|> void (char '(')
  return pos
