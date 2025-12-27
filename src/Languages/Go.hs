module Languages.Go (extensions, searchPath, symbolAtPosition, findSymbolDefinition) where

import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Languages.Common (FileFilter (..), Parser, atDepth, hasAnyExt, hasAnyFilename, none, not_, recurseDirectory, searchForMatch, symbolAtPos, whenDir, whenFile)
import System.OsPath (OsString)
import Text.Megaparsec (SourcePos, getSourcePos, optional, satisfy, takeWhile1P, (<|>))
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Types (IrkFile (..), IrkFileArea (..), IrkFilePos (..))
import Utils (os)

extensions :: [OsString]
extensions = [os ".go"]

fileFilter :: FileFilter
fileFilter =
  mconcat
    [ whenFile $ hasAnyExt extensions,
      whenDir (atDepth 1 $ not_ $ hasAnyFilename [os "vendor"])
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
isIdentifierChar ch = isAlphaNum ch || ch == '_'

-- | Validate an identifier, with the assumption that all characters
-- | follow 'isIdentifierChar'.
isIdentifier :: Text -> Bool
isIdentifier i = maybe False (not . isDigit . fst) $ T.uncons i

findSymbolDefinition :: Text -> Text -> [IrkFilePos]
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
