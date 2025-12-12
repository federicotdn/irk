module Languages.Haskell (extensions, searchPaths, symbolAtPosition, findSymbolDefinition) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Languages.Common (Parser, PathFilter, hasAnyExtension, hasAnyFilename, recurseDirectories, searchForMatch, symbolAtPos)
import System.OsPath (OsString)
import Text.Megaparsec (SourcePos, getSourcePos, (<|>))
import Text.Megaparsec.Char (char, space, space1, string)
import Utils (FileKind (..), FilePathKind (..), FilePos, Search (..), filePathWithKind, os)

extensions :: [OsString]
extensions = [os ".hs"]

pathFilter :: PathFilter
pathFilter _ path False = hasAnyExtension path extensions
pathFilter 1 path True = not (hasAnyFilename path [os "vendor"])
pathFilter _ _ _ = True

pathFilterVendor :: PathFilter
pathFilterVendor 1 _ False = False -- Ignore top-level files
pathFilterVendor 1 path True = not (pathFilter 1 path True) -- Recurse into vendor/
pathFilterVendor depth path isDir = pathFilter depth path isDir

searchPaths :: Search -> IO [FilePathKind]
searchPaths search = do
  case search of
    WorkspaceSearch workspaces -> do
      paths <- recurseDirectories pathFilter workspaces
      return $ map (filePathWithKind Workspace) paths
    WorkspaceVendoredSearch workspaces -> do
      paths <- recurseDirectories pathFilterVendor workspaces
      return $ map (filePathWithKind WorkspaceVendored) paths
    ExternalSearch -> return []

symbolAtPosition :: Text -> FilePos -> Maybe Text
symbolAtPosition = symbolAtPos isIdentifierChar isIdentifier

isIdentifierChar :: Char -> Bool
isIdentifierChar ch = isAlphaNum ch || (ch `elem` ['\'', '_'])

-- | Validate an identifier, with the assumption that all characters
-- | follow 'isIdentifierChar'.
isIdentifier :: Text -> Bool
isIdentifier i = maybe False (not . isDigit . fst) $ T.uncons i

findSymbolDefinition :: Text -> Text -> [FilePos]
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
