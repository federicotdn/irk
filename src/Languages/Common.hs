module Languages.Common
  ( recurseDirectories,
    Parser,
    searchForMatch,
    symbolAtPos,
    PathFilter,
    hasAnyExtension,
    hasAnyFilename,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (concurrently)
import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import System.Directory.OsPath (doesDirectoryExist, listDirectory)
import System.OsPath (OsPath, pack, pathSeparator, takeFileName)
import System.OsString (OsString, isSuffixOf)
import Text.Megaparsec (Parsec, atEnd, optional, parse, takeWhileP, try)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Pos (SourcePos, sourceColumn, sourceLine, unPos)
import Utils (FilePos (..), extractLine, oss, splitList)

type Parser = Parsec Void Text

type PathFilter = Int -> OsPath -> Bool -> Bool

hasAnyExtension :: OsPath -> [OsString] -> Bool
hasAnyExtension path = any (`isSuffixOf` path)

hasAnyFilename :: OsPath -> [OsPath] -> Bool
hasAnyFilename path names = takeFileName path `elem` names

baseFilter :: PathFilter
baseFilter _ path True = not (hasAnyFilename path $ oss [".git", ".github", ".svn", ".hg", ".yarn", ".nx"])
baseFilter _ _ _ = True
{-# INLINE baseFilter #-}

sep :: OsString
sep = pack [pathSeparator]

-- Avoid using OsPath </> since it does too much work.
joinPaths :: OsPath -> OsPath -> OsPath
joinPaths p1 p2 = mconcat [p1, sep, p2]
{-# INLINE joinPaths #-}

recurseDirectory :: Int -> PathFilter -> OsPath -> IO [OsPath]
recurseDirectory depth filterby dir = do
  isDir <- doesDirectoryExist dir
  let process = baseFilter depth dir isDir && filterby depth dir isDir
  if isDir && process
    then do
      listing <- listDirectory dir
      capa <- getNumCapabilities
      -- Max. possible of active threads searching at this depth
      let maxCurrThreads = 2 ^ depth :: Int
      let paths = map (dir `joinPaths`) listing
      let depth' = depth + 1
      -- Only use parallelism (two threads) if:
      -- - We have currently not exceeded (probably) #getNumCapabilities threads
      -- - The list of paths contains two or more items
      sublistings <-
        if maxCurrThreads < capa && length paths > 1
          then do
            let (pathsA, pathsB) = splitList paths
            (r1, r2) <-
              concurrently
                (recurseDirectoriesInner depth' filterby pathsA)
                (recurseDirectoriesInner depth' filterby pathsB)
            return [r1, r2]
          else mapM (recurseDirectory depth' filterby) paths
      return $ concat sublistings
    else if process then return [dir] else return []
{-# INLINE recurseDirectory #-}

recurseDirectoriesInner :: Int -> PathFilter -> [OsPath] -> IO [OsPath]
recurseDirectoriesInner depth filterby dirs = concat <$> mapM (recurseDirectory depth filterby) dirs
{-# INLINE recurseDirectoriesInner #-}

recurseDirectories :: PathFilter -> [OsPath] -> IO [OsPath]
recurseDirectories filterby dirs = concat <$> mapM (recurseDirectory 0 filterby) dirs
{-# INLINE recurseDirectories #-}

skipLine :: Parser ()
skipLine = do
  _ <- takeWhileP Nothing (/= '\n')
  _ <- optional newline
  return ()

searchForMatchInner :: Parser SourcePos -> Parser [FilePos]
searchForMatchInner parser = loop []
  where
    loop matches = do
      isAtEnd <- atEnd
      if isAtEnd
        then return matches
        else do
          matched <- optional (try parser)
          newMatches <- case matched of
            Just pos -> do
              let line = unPos (sourceLine pos) - 1
              let column = unPos (sourceColumn pos) - 1
              return [FilePos Nothing line column]
            Nothing -> return []
          _ <- skipLine
          loop (matches ++ newMatches)
{-# INLINE searchForMatchInner #-}

searchForMatch :: Parser SourcePos -> Text -> [FilePos]
searchForMatch parser source =
  case parse (searchForMatchInner parser) "<input>" source of
    Right result -> result
    _ -> []
{-# INLINE searchForMatch #-}

symbolAtPos :: (Char -> Bool) -> (Text -> Bool) -> Text -> FilePos -> Maybe Text
symbolAtPos isIdentifierChar isIdentifier source fp = case parts of
  Nothing -> Nothing
  Just (before, after) -> do
    let prefix = T.takeWhileEnd isIdentifierChar before
    let suffix = T.takeWhile isIdentifierChar after
    let result = prefix <> suffix
    guard (not $ T.null result)
    guard $ isIdentifier result
    return result
  where
    parts = extractLine source fp
{-# INLINE symbolAtPos #-}
