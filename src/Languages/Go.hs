module Languages.Go (extensions, searchPaths, symbolAtPosition, findSymbolDefinition) where

import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Languages.Common (Parser, PathFilter, hasAnyExtension, hasAnyFilename, recurseDirectories, searchForMatch, symbolAtPos)
import System.OsPath (OsString)
import Text.Megaparsec (SourcePos, getSourcePos, optional, takeWhile1P, (<|>), satisfy)
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Utils (FileKind (..), FilePathKind (..), FilePos, Search (..), filePathWithKind, os)

extensions :: [OsString]
extensions = [os ".go"]

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
isIdentifierChar ch = isAlphaNum ch || ch == '_'

-- | Validate an identifier, with the assumption that all characters
-- | follow 'isIdentifierChar'.
isIdentifier :: Text -> Bool
isIdentifier i = maybe False (not . isDigit . fst) $ T.uncons i

findSymbolDefinition :: Text -> Text -> [FilePos]
findSymbolDefinition symbol = searchForMatch $ findFuncDef symbol <|> findTypeDef symbol

findTypeDef :: Text -> Parser SourcePos
findTypeDef name = do
  _ <- string "type"
  _ <- hspace1
  pos <- getSourcePos
  _ <- string name
  _ <- satisfy (`elem` [' ', '[', '='])
  return pos

findFuncDef :: Text -> Parser SourcePos
findFuncDef name = do
  -- There could be spaces before 'func' but this never
  -- happens in practice.
  _ <- string "func"
  _ <- hspace1
  _ <- optional $ do
    _ <- char '('
    _ <- takeWhile1P Nothing (/= ')')
    _ <- char ')'
    _ <- hspace1
    pure ()
  pos <- getSourcePos
  _ <- string name
  _ <- optional $ do
    _ <- char '['
    _ <- takeWhile1P Nothing (/= ']')
    _ <- char ']'
    pure ()
  _ <- hspace
  _ <- char '('
  _ <- takeWhile1P Nothing (/= '{')
  _ <- char '{'
  return pos
