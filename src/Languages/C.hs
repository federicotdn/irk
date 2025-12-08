module Languages.C (extensions, searchPaths, symbolAtPosition, findSymbolDefinition) where

import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Languages.Common (Parser, PathFilter, hasAnyExtension, recurseDirectories, searchForMatch, symbolAtPos)
import System.OsPath (OsString)
import Text.Megaparsec (SourcePos, getSourcePos, takeWhile1P, takeWhileP, (<|>))
import Text.Megaparsec.Char (char, hspace1, space, space1, string)
import Utils (FileKind (..), FilePathKind (..), FilePos (..), Search (..), filePathWithKind, oss)

extensions :: [OsString]
extensions = oss [".c", ".h"]

pathFilter :: PathFilter
pathFilter _ path False = hasAnyExtension path extensions
pathFilter _ _ _ = True

searchPaths :: Search -> IO [FilePathKind]
searchPaths search = do
  case search of
    WorkspaceSearch workspaces -> do
      paths <- recurseDirectories pathFilter workspaces
      return $ map (filePathWithKind Workspace) paths
    _ -> error "unimplemented"

symbolAtPosition :: Text -> FilePos -> Maybe Text
symbolAtPosition = symbolAtPos isIdentifierChar isIdentifier

isIdentifierChar :: Char -> Bool
isIdentifierChar ch = isAlphaNum ch || ch == '_'

-- | Validate an identifier, with the assumption that all characters
-- | follow 'isIdentifierChar'.
isIdentifier :: Text -> Bool
isIdentifier i = maybe False (not . isDigit . fst) $ T.uncons i

findSymbolDefinition :: Text -> Text -> [FilePos]
findSymbolDefinition symbol = searchForMatch $ findMacroDef symbol <|> findFuncDef symbol

findMacroDef :: Text -> Parser SourcePos
findMacroDef name = do
  _ <- string "#define"
  _ <- hspace1
  pos <- getSourcePos
  _ <- string name
  _ <- hspace1
  return pos

findFuncDef :: Text -> Parser SourcePos
findFuncDef name = do
  _ <- takeWhileP Nothing (\c -> c == ' ' || c == '\t')
  _ <- takeWhile1P Nothing isIdentifierChar
  _ <- space1
  pos <- getSourcePos
  _ <- string name
  _ <- space
  _ <- char '('
  _ <- takeWhileP Nothing (/= ')')
  _ <- char ')'
  _ <- space
  _ <- char '{'
  return pos
