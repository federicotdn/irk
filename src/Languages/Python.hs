module Languages.Python
  ( extensions,
    searchPath,
    symbolAtPosition,
    findSymbolDefinition,
    processResults,
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
  )
import System.OsPath (OsString, joinPath, splitDirectories)
import Text.Megaparsec
  ( SourcePos,
    choice,
    getSourcePos,
    optional,
    takeWhile1P,
    takeWhileP,
    try,
  )
import Text.Megaparsec.Char (char, hspace, hspace1, string)
import Types (IrkFile (..), IrkFileArea (..), IrkFilePos (..))
import Utils (os)

extensions :: [OsString]
extensions = [os ".py"]

ignore :: Ignore
ignore =
  baseIgnore
    <> parse
      "\
      \ /.venv/   \n\
      \ /.env/    \n\
      \ /venv/    \n\
      \ /env/     \n\
      \ !*.py     \n\
      \"

ignoreForVendor :: Ignore
ignoreForVendor =
  baseIgnore
    <> parse
      "\
      \ /*/        \n\
      \ !/.venv/   \n\
      \ !/.env/    \n\
      \ !/venv/    \n\
      \ !/env/     \n\
      \ !*.py      \n\
      \"

searchPath :: IrkFile -> IO [IrkFile]
searchPath origin = do
  case iArea origin of
    Workspace -> recurseDirectory ignore origin
    WorkspaceVendored -> recurseDirectory ignoreForVendor origin
    External -> return []

symbolAtPosition :: Text -> IrkFilePos -> Maybe Text
symbolAtPosition = symbolAtPos isIdentifierChar isIdentifier

isIdentifierChar :: Char -> Bool
isIdentifierChar ch = isAlphaNum ch || ch == '_'

-- | Validate an identifier, with the assumption that all characters follow 'isIdentifierChar'.
isIdentifier :: Text -> Bool
isIdentifier i = maybe False (not . isDigit . fst) $ T.uncons i

findSymbolDefinition :: Text -> Text -> [IrkFilePos]
findSymbolDefinition symbol =
  searchForMatch $
    choice
      [ findTopLevelAssignment symbol,
        -- Use 'try' here to avoid consuming initial whitespace in case we don't match.
        try (findFuncDef symbol),
        findClassDef symbol
      ]

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

processResults :: [IrkFilePos] -> [IrkFilePos]
processResults positions = filter include positions
  where
    include (IrkFilePos IrkFile {iPath = path} _ _) =
      let replaced = replaceDir path (os "lib64") (os "lib")
       in replaced `notElem` paths || replaced == path
    paths = map (\(IrkFilePos IrkFile {iPath = path} _ _) -> path) positions
    replaceDir path from to = joinPath $ map (\part -> if part == from then to else part) (splitDirectories path)
