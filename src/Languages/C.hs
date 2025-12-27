module Languages.C (extensions, searchPath, symbolAtPosition, findSymbolDefinition) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Languages.Common (FileFilter (..), Parser, hasAnyExt, recurseDirectory, searchForMatch, symbolAtPos, whenFile)
import System.OsPath (OsString)
import Text.Megaparsec (SourcePos, getSourcePos, optional, takeWhile1P, takeWhileP, (<|>))
import Text.Megaparsec.Char (char, hspace1, space, space1, string)
import Types (IrkFile (..), IrkFileArea (..), IrkFilePos (..))
import Utils (oss)

extensions :: [OsString]
extensions = oss [".c", ".h"]

fileFilter :: FileFilter
fileFilter = whenFile $ hasAnyExt extensions

searchPath :: IrkFile -> IO [IrkFile]
searchPath origin = do
  case iArea origin of
    Workspace -> do
      recurseDirectory fileFilter origin
    WorkspaceVendored -> return []
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
findSymbolDefinition symbol = searchForMatch $ findMacroDef symbol <|> findFuncDef symbol

findMacroDef :: Text -> Parser SourcePos
findMacroDef name = do
  _ <- string "#define"
  _ <- hspace1
  pos <- getSourcePos
  _ <- string name
  _ <- hspace1 <|> void (char '(')
  return pos

findFuncDef :: Text -> Parser SourcePos
findFuncDef name = do
  _ <- takeWhileP Nothing (\c -> c == ' ' || c == '\t')
  _ <- optional $ do
    _ <- string "static"
    _ <- space1
    pure ()
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
