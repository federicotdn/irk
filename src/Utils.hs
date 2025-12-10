module Utils
  ( ePutStrLn,
    fileText,
    extractLine,
    FileKind (..),
    FilePathKind (..),
    Search (..),
    FilePos (..),
    filePosWithPath,
    filePathWithKind,
    os,
    oss,
    splitInto,
    longestPrefix,
    forConcurrentlyMax,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.List (maximumBy)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import System.OsString (OsString, isPrefixOf)
import qualified System.OsString as OS

-- | os converts a 'String' instance to 'OsString'.
-- | Only for use with literals.
os :: String -> OsString
os = unsafeEncodeUtf

-- | Like 'os', but for lists of 'String'.
oss :: [String] -> [OsString]
oss = map os

-- | Represents a text file position: path, line and column/char.
-- | Both values are 0-indexed.
data FilePos = FilePos (Maybe OsPath) Int Int deriving (Show, Eq)

filePosWithPath :: FilePos -> OsPath -> FilePos
filePosWithPath (FilePos _ l c) path = FilePos (Just path) l c

data Search
  = WorkspaceSearch [OsPath]
  | WorkspaceVendoredSearch [OsPath]
  | ExternalSearch

data FileKind
  = Current -- Current file
  | Workspace -- Files in a workspace
  | WorkspaceVendored -- Vendored files in a workspace
  | External -- Files external to all workspaces (global/system)
  deriving (Show, Eq)

data FilePathKind = FilePathKind OsPath FileKind deriving (Show, Eq)

filePathWithKind :: FileKind -> OsPath -> FilePathKind
filePathWithKind k path = FilePathKind path k

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

fileText :: OsPath -> IO (Maybe Text)
fileText path = do
  path' <- decodeUtf path
  result <- tryIOError (BS.readFile path')
  case result of
    Right content -> return $ Just (TE.decodeUtf8Lenient content)
    Left _ -> return Nothing

-- TODO: Fix splitAt assuming UTF32
extractLine :: Text -> FilePos -> Maybe (Text, Text)
extractLine t (FilePos _ l c) = case drop l (T.lines t) of
  [] -> Nothing
  ls -> Just $ T.splitAt c (head ls)

longestPrefix :: OsString -> [OsString] -> Maybe OsPath
longestPrefix path prefixes =
  case filter (`isPrefixOf` path) prefixes of
    [] -> Nothing
    matches -> Just $ maximumBy (compare `on` OS.length) matches

splitInto :: Int -> [a] -> [[a]]
splitInto n xs = go n (length xs) xs
  where
    go _ _ [] = []
    go 1 _ ys = [ys]
    go m len ys =
      let chunkSize = (len + m - 1) `div` m
          (chunk, rest) = splitAt chunkSize ys
       in chunk : go (m - 1) (len - length chunk) rest

forConcurrentlyMax :: [a] -> (a -> IO b) -> IO [b]
forConcurrentlyMax elems f = do
  capa <- getNumCapabilities
  let parts = splitInto capa elems
  results <- mapConcurrently (mapM f) parts
  return $ concat results
