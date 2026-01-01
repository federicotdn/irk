module Languages.Common
  ( recurseDirectory,
    Parser,
    searchForMatch,
    symbolAtPos,
    baseIgnore,
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
import Ignore (Ignore)
import qualified Ignore as I
import System.Directory.Internal
  ( FileType (..),
    fileSizeFromMetadata,
    fileTypeFromMetadata,
    getFileMetadata,
  )
import System.Directory.OsPath (listDirectory)
import Text.Megaparsec (Parsec, atEnd, optional, parse, takeWhileP, try)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Pos (SourcePos, sourceColumn, sourceLine, unPos)
import Types (IrkFile (..), IrkFilePos (..), file)
import Utils (extractLine, sep)

type Parser = Parsec Void Text

baseIgnore :: Ignore
baseIgnore =
  I.parse
    ( T.unlines
        [ "*", --   Ignore all files at all depths.
          "!*/", -- Un-ignore all directories so that we can recurse into them.
          ".*/" -- Re-ignore all hidden directories (.foo) at all depths.
        ]
    )

irkFileIgnored :: Ignore -> IrkFile -> Bool
irkFileIgnored ign f = I.ignores ign (Right $ iRelPathParts f) (iDir f)

recurseDirectory :: Ignore -> IrkFile -> IO [IrkFile]
recurseDirectory ign dir = do
  queue <- newTQueueIO
  pending <- newTVarIO (1 :: Int)
  results <- newTQueueIO
  capa <- getNumCapabilities
  -- Avoid using OsPath </> since it does too much work.
  let fastJoinPaths p1 p2 = mconcat [p1, sep, p2]

  atomically $
    writeTQueue queue $
      IrkFile
        { iPath = iPath dir,
          iRelPathParts = [],
          iDir = True,
          iFileSize = Nothing,
          iDepth = 0,
          iArea = iArea dir
        }

  let worker = do
        (mnext, cpending) <- atomically $ do
          mnext <- tryReadTQueue queue
          cpending <- readTVar pending
          -- Worker did not get anything from the queue, but
          -- there are still other workers processing directories.
          -- There could be more work to do soon - retry.
          when (isNothing mnext && cpending > 0) retry
          return (mnext, cpending)

        case mnext of
          Nothing -> pure ()
          Just next -> do
            let depth' = iDepth next + 1
            entries <- listDirectory $ iPath next
            entries' <- forM entries $ \e -> do
              let path = iPath next `fastJoinPaths` e
              metadata <- getFileMetadata path
              let isDir = fileTypeFromMetadata metadata `elem` [Directory, DirectoryLink]
              let fileSize = if isDir then Nothing else Just $ fileSizeFromMetadata metadata
              return
                IrkFile
                  { iPath = path,
                    iRelPathParts = iRelPathParts next ++ [e],
                    iDir = isDir,
                    iFileSize = fileSize,
                    iDepth = depth',
                    iArea = iArea next
                  }

            let (p1, p2) = partition iDir entries'
            let directories = filter (not . irkFileIgnored ign) p1
            let files = filter (not . irkFileIgnored ign) p2

            -- Write the directories to the queue now so that another
            -- thread can pick up more work.
            atomically $ do
              unless (null directories) $ mapM_ (writeTQueue queue) directories
              -- The -1 here is from the fact that we have already
              -- processed a value we removed ('mnext').
              modifyTVar' pending (+ (length directories - 1))

            -- If files were read, add them to 'results' in a separate
            -- transaction.
            unless (null files) $ atomically $ do
              mapM_ (writeTQueue results) files

        when (cpending > 0) worker

  replicateConcurrently_ capa worker

  atomically $ do
    p <- readTVar pending
    when (p > 0) retry
    flushTQueue results
{-# INLINE recurseDirectory #-}

skipLine :: Parser ()
skipLine = do
  _ <- takeWhileP Nothing (/= '\n')
  _ <- optional newline
  return ()

searchForMatchInner :: Parser SourcePos -> Parser [IrkFilePos]
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
              return [IrkFilePos file line column]
            Nothing -> return []
          _ <- skipLine
          loop (matches ++ newMatches)
{-# INLINE searchForMatchInner #-}

searchForMatch :: Parser SourcePos -> Text -> [IrkFilePos]
searchForMatch parser source =
  case parse (searchForMatchInner parser) "<input>" source of
    Right result -> result
    _ -> []
{-# INLINE searchForMatch #-}

symbolAtPos :: (Char -> Bool) -> (Text -> Bool) -> Text -> IrkFilePos -> Maybe Text
symbolAtPos isIdentifierChar isIdentifier source (IrkFilePos _ line col) = case parts of
  Nothing -> Nothing
  Just (before, after) -> do
    let prefix = T.takeWhileEnd isIdentifierChar before
    let suffix = T.takeWhile isIdentifierChar after
    let result = prefix <> suffix
    guard (not $ T.null result)
    guard $ isIdentifier result
    return result
  where
    parts = extractLine source line col
{-# INLINE symbolAtPos #-}
