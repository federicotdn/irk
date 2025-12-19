module Languages.Python (extensions, searchPath, symbolAtPosition, findSymbolDefinition) where

import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Languages.Common (Parser, PathFilter, hasAnyExtension, hasAnyFilename, recurseDirectory, searchForMatch, symbolAtPos)
import System.OsPath (OsString)
import Text.Megaparsec (SourcePos, getSourcePos, optional, takeWhile1P, takeWhileP, try, (<|>))
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Types (IrkFile (..), IrkFileArea (..), IrkFilePos (..))
import Utils (os, oss)

extensions :: [OsString]
extensions = [os ".py"]

pathFilter :: PathFilter
pathFilter _ path False = hasAnyExtension path extensions
pathFilter 1 path True = not (hasAnyFilename path $ oss [".env", ".venv", "env", "venv"])
pathFilter _ _ _ = True

pathFilterVendor :: PathFilter
pathFilterVendor 1 _ False = False -- Ignore top-level files
pathFilterVendor 1 path True = not (pathFilter 1 path True) -- Recurse into venvs
pathFilterVendor depth path isDir = pathFilter depth path isDir

searchPath :: IrkFile -> IO [IrkFile]
searchPath origin = do
  case iArea origin of
    Workspace -> recurseDirectory pathFilter origin
    WorkspaceVendored -> recurseDirectory pathFilterVendor origin
    External -> return []

symbolAtPosition :: Text -> IrkFilePos -> Maybe Text
symbolAtPosition = symbolAtPos isIdentifierChar isIdentifier

isIdentifierChar :: Char -> Bool
isIdentifierChar ch = isAlphaNum ch || ch == '_'

-- | Validate an identifier, with the assumption that all characters
-- | follow 'isIdentifierChar'.
isIdentifier :: Text -> Bool
isIdentifier i = maybe False (not . isDigit . fst) $ T.uncons i

findSymbolDefinition :: Text -> Text -> [IrkFilePos]
findSymbolDefinition symbol =
  searchForMatch $
    findTopLevelAssignment symbol
      <|>
      -- Use 'try' here to avoid consuming initial whitespace in case we don't match
      try (findFuncDef symbol)
      <|> findClassDef symbol

findTopLevelAssignment :: Text -> Parser SourcePos
findTopLevelAssignment name = do
  pos <- getSourcePos
  _ <- string name
  _ <- hspace
  _ <- char '='
  _ <- hspace
  return pos

findClassDef :: Text -> Parser SourcePos
findClassDef name = do
  _ <- takeWhileP Nothing (\c -> c == ' ' || c == '\t')
  _ <- string "class"
  _ <- hspace1
  pos <- getSourcePos
  _ <- string name
  _ <- hspace
  _ <- optional $ do
    _ <- char '('
    _ <- takeWhileP Nothing (/= ')')
    _ <- char ')'
    _ <- hspace
    pure ()
  _ <- char ':'
  return pos

findFuncDef :: Text -> Parser SourcePos
findFuncDef name = do
  _ <- takeWhileP Nothing (\c -> c == ' ' || c == '\t')
  _ <- string "def"
  _ <- hspace1
  pos <- getSourcePos
  _ <- string name
  _ <- hspace
  _ <- char '('
  _ <- takeWhile1P Nothing (/= ':')
  _ <- char ':'
  return pos
