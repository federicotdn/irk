module Irk where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TQueue (flushTQueue, isEmptyTQueue, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Infix (isInfixOfC)
import Language (Language, lFindSymbolDefinition, lSearchPath, lSymbolAtPosition)
import System.OsPath (OsPath)
import System.OsString (empty)
import Types (IrkFile (..), IrkFileArea (..), IrkFilePos (..), file, workspaceRoot)
import Utils (fileByteString, longestPrefix)

searchPaths :: Language -> Maybe OsPath -> [OsPath] -> [IO [IrkFile]]
searchPaths lang mcurrent workspaces =
  let current = fromMaybe empty mcurrent
      mactiveWorkspace = longestPrefix current workspaces
      otherWorkspaces = filter (\w -> Just w /= mactiveWorkspace) workspaces
      currentPath = maybe [] (\path -> [file {iPath = path}]) mcurrent
   in -- Don't actually run the file searches, but rather build the
      -- IO [FilePathKind] that can be later evaluated to get a list
      -- of paths.
      pure currentPath
        : map (lSearchPath lang . workspaceRoot Workspace) (maybeToList mactiveWorkspace)
        ++ map (lSearchPath lang . workspaceRoot WorkspaceVendored) (maybeToList mactiveWorkspace)
        ++ map (lSearchPath lang . workspaceRoot Workspace) otherWorkspaces
        ++ map (lSearchPath lang . workspaceRoot WorkspaceVendored) otherWorkspaces
        ++ [lSearchPath lang file {iArea = External}]

symbolAtPosition :: Language -> Text -> IrkFilePos -> Maybe Text
symbolAtPosition = lSymbolAtPosition

findSymbolDefinitionInPath :: Language -> Text -> BS.ByteString -> IrkFile -> IO [IrkFilePos]
findSymbolDefinitionInPath lang symbol symbolBS f = do
  msource <- fileByteString f
  case msource of
    Just source -> do
      -- Do a quick ByteString within ByteString check first before
      -- running the full parser. This helps with performance because
      -- for the majority of the files scanned, the symbol will not be
      -- contained in them.
      if symbolBS `isInfixOfC` source
        then return $ map (\(IrkFilePos _ l c) -> IrkFilePos f l c) $ lFindSymbolDefinition lang symbol (decodeUtf8Lenient source)
        else return []
    _ -> return []

findSymbolDefinitionInPaths :: Language -> Text -> [IrkFile] -> IO [IrkFilePos]
findSymbolDefinitionInPaths lang symbol paths = do
  queue <- newTQueueIO
  results <- newTQueueIO
  capa <- getNumCapabilities
  let symbolBS = encodeUtf8 symbol

  atomically $ mapM_ (writeTQueue queue) paths

  let worker = do
        mnext <- atomically $ tryReadTQueue queue
        case mnext of
          Just next -> do
            positions <- findSymbolDefinitionInPath lang symbol symbolBS next
            atomically $ do
              mapM_ (writeTQueue results) positions
            worker
          Nothing -> pure ()

  replicateConcurrently_ capa worker

  atomically $ do
    isEmpty <- isEmptyTQueue queue
    unless isEmpty retry
    flushTQueue results

findSymbolDefinition :: Language -> Text -> [IO [IrkFile]] -> IO [IrkFilePos]
findSymbolDefinition lang symbol searches = do
  case searches of
    [] -> return []
    search : rest -> do
      paths <- search -- Evaluate the file search now.
      positions <- findSymbolDefinitionInPaths lang symbol paths
      case positions of
        [] -> findSymbolDefinition lang symbol rest
        _ -> return positions
