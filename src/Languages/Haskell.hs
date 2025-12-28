module Languages.Haskell (extensions, searchPath, symbolAtPosition, findSymbolDefinition) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Languages.Common (FileFilter, Parser, atDepth, hasAnyExt, hasAnyFilename, none, notWhen, recurseDirectory, searchForMatch, symbolAtPos, whenDir, whenFile)
import System.OsPath (OsString)
import Text.Megaparsec (SourcePos, getSourcePos, (<|>))
import Text.Megaparsec.Char (char, space, space1, string)
import Types (IrkFile (..), IrkFileArea (..), IrkFilePos (..))
import Utils (os, oss)

extensions :: [OsString]
extensions = oss [".hs", ".hsc"]

fileFilter :: FileFilter
fileFilter =
  mconcat
    [ whenFile $ hasAnyExt extensions,
      whenDir (atDepth 1 $ notWhen $ hasAnyFilename [os "vendor"])
    ]

fileFilterVendor :: FileFilter
fileFilterVendor =
  mconcat
    [ whenFile (hasAnyExt extensions <> atDepth 1 none),
      whenDir (atDepth 1 $ hasAnyFilename [os "vendor"])
    ]

searchPath :: IrkFile -> IO [IrkFile]
searchPath origin = do
  case iArea origin of
    Workspace -> recurseDirectory fileFilter origin
    WorkspaceVendored -> recurseDirectory fileFilterVendor origin
    External -> return []

symbolAtPosition :: Text -> IrkFilePos -> Maybe Text
symbolAtPosition = symbolAtPos isIdentifierChar isIdentifier

isIdentifierChar :: Char -> Bool
isIdentifierChar ch = isAlphaNum ch || (ch `elem` ['\'', '_'])

-- | Validate an identifier, with the assumption that all characters
-- | follow 'isIdentifierChar'.
isIdentifier :: Text -> Bool
isIdentifier i = maybe False (not . isDigit . fst) $ T.uncons i

findSymbolDefinition :: Text -> Text -> [IrkFilePos]
findSymbolDefinition symbol = searchForMatch $ findDef symbol <|> findTypeDef symbol <|> findClassDef symbol <|> findModuleDef symbol

findDef :: Text -> Parser SourcePos
findDef name = do
  pos <- getSourcePos
  _ <- string name
  _ <- space
  _ <- string "::"
  return pos

findTypeDef :: Text -> Parser SourcePos
findTypeDef name = do
  _ <- string "data" <|> string "newtype" <|> string "type"
  _ <- space1
  pos <- getSourcePos
  _ <- string name
  _ <- space1 <|> void (char '=')
  return pos

findClassDef :: Text -> Parser SourcePos
findClassDef name = do
  -- TODO: Not correct
  _ <- string "class"
  _ <- space1
  pos <- getSourcePos
  _ <- string name
  _ <- space1
  return pos

findModuleDef :: Text -> Parser SourcePos
findModuleDef name = do
  _ <- string "module"
  _ <- space1
  pos <- getSourcePos
  _ <- string name
  _ <- space1 <|> void (char '(')
  return pos
