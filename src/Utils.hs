module Utils
  ( ePutStrLn,
    fileText,
    fileByteString,
    extractLine,
    os,
    oss,
    osc,
    longestPrefix,
    ignoreIOError,
    isWindowsAbs,
  )
where

import qualified Data.ByteString as BS
import Data.Function (on)
import Data.List (maximumBy)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory.OsPath (getFileSize)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)
import System.IO.MMap (mmapFileByteString)
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf, unsafeFromChar)
import System.OsString (OsChar, OsString, isPrefixOf)
import qualified System.OsString as OS
import Types (IrkFile (..), IrkFilePos (..))

-- File size thresholds for reading strategy
maxFileSize :: Integer
maxFileSize = 4 * 1024 * 1024 -- 4 MB

mmapThreshold :: Integer
mmapThreshold = 32 * 1024 -- 32 KB

-- | os converts a 'String' instance to 'OsString'.
-- | Only for use with literals.
os :: String -> OsString
os = unsafeEncodeUtf

-- | Like 'os', but for lists of 'String'.
oss :: [String] -> [OsString]
oss = map os

-- | Like 'os', but for 'Char'.
osc :: Char -> OsChar
osc = unsafeFromChar

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

fileByteString :: IrkFile -> IO (Maybe BS.ByteString)
fileByteString f = do
  path <- decodeUtf $ iPath f
  size <- case iFileSize f of
    Just s -> pure s
    Nothing -> getFileSize $ iPath f
  case size of
    0 -> return Nothing
    _
      | size > maxFileSize -> return Nothing
      | size < mmapThreshold -> ignoreIOError (BS.readFile path)
      | otherwise -> ignoreIOError (mmapFileByteString path Nothing)

fileText :: IrkFile -> IO (Maybe Text)
fileText path = do
  contents <- fileByteString path
  return (TE.decodeUtf8Lenient <$> contents)

-- TODO: Fix splitAt assuming UTF32
extractLine :: Text -> IrkFilePos -> Maybe (Text, Text)
extractLine t (IrkFilePos _ l c) = case drop l (T.lines t) of
  [] -> Nothing
  ls -> Just $ T.splitAt c (head ls)

longestPrefix :: OsString -> [OsString] -> Maybe OsPath
longestPrefix path prefixes =
  case filter (`isPrefixOf` path) prefixes of
    [] -> Nothing
    matches -> Just $ maximumBy (compare `on` OS.length) matches

ignoreIOError :: IO a -> IO (Maybe a)
ignoreIOError op = do
  result <- tryIOError op
  case result of
    Right val -> return $ Just val
    Left _ -> return Nothing

-- | Checks whether a path appears like 'C:/some/path'.
isWindowsAbs :: OsPath -> Bool
isWindowsAbs uri =
  OS.length uri >= 2
    && OS.toChar (OS.head uri) `elem` ['a' .. 'z'] ++ ['A' .. 'Z']
    && OS.toChar (uri `OS.index` 1) == ':'
