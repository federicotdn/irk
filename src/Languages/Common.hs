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
import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TQueue (flushTQueue, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (modifyTVar', newTVarIO, readTVar)
import Control.Monad (forM, guard, unless, when)
import Data.List (partition)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import System.Directory.OsPath (doesDirectoryExist, listDirectory)
import System.OsPath (OsPath, pack, pathSeparator, takeFileName)
import System.OsString (OsString, isSuffixOf)
import Text.Megaparsec (Parsec, atEnd, optional, parse, takeWhileP, try)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Pos (SourcePos, sourceColumn, sourceLine, unPos)
import Utils (FilePos (..), extractLine, oss)

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

recurseDirectory :: PathFilter -> OsPath -> IO [OsPath]
recurseDirectory filterby dir = do
  queue <- newTQueueIO
  pending <- newTVarIO (1 :: Int)
  results <- newTQueueIO
  capa <- getNumCapabilities

  atomically $ writeTQueue queue (0 :: Int, dir)

  let worker = do
        (mnext, cpending) <- atomically $ do
          mnext <- tryReadTQueue queue
          cpending <- readTVar pending
          when (isNothing mnext && cpending > 0) retry
          return (mnext, cpending)

        case mnext of
          Nothing -> pure ()
          Just (depth, next) -> do
            entries <- listDirectory next
            entries' <- forM entries $ \e -> do
              let path = next `joinPaths` e
              isDir <- doesDirectoryExist path
              return (isDir, path)

            let depth' = depth + 1
            let (p1, p2) = partition fst entries'
            let directories = filter (\d -> baseFilter depth' d True && filterby depth' d True) (map snd p1)
            let files = filter (\f -> baseFilter depth' f False && filterby depth' f False) (map snd p2)

            atomically $ do
              unless (null files) $ mapM_ (writeTQueue results) files
              unless (null directories) $ mapM_ (writeTQueue queue . (,) depth') directories
              -- The -1 here is from the fact that we have already
              -- processed a value we removed ('mnext').
              modifyTVar' pending (+ (length directories - 1))

        when (cpending > 0) worker

  replicateConcurrently_ capa worker

  atomically $ do
    p <- readTVar pending
    when (p > 0) retry
    flushTQueue results
{-# INLINE recurseDirectory #-}

recurseDirectories :: PathFilter -> [OsPath] -> IO [OsPath]
recurseDirectories filterby dirs = concat <$> mapM (recurseDirectory filterby) dirs
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
