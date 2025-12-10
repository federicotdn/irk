module Irk where

import Control.Monad (forM, join)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text, isInfixOf)
import Language (Language, lFindSymbolDefinition, lSearchPaths, lSymbolAtPosition)
import System.OsPath (OsPath)
import System.OsString (empty)
import Utils (FileKind (..), FilePathKind (..), FilePos, Search (..), filePosWithPath, fileText, forConcurrentlyMax, longestPrefix)

searchPaths :: Language -> Maybe OsPath -> [OsPath] -> [IO [FilePathKind]]
searchPaths lang mcurrent workspaces =
  let current = fromMaybe empty mcurrent
      mactiveWorkspace = longestPrefix current workspaces
      otherWorkspaces = filter (\w -> Just w /= mactiveWorkspace) workspaces
      currentPath = maybe [] (\path -> [FilePathKind path Current]) mcurrent
   in -- Don't actually run the file searches, but rather build the
      -- IO [FilePathKind] that can be later evaluated to get a list
      -- of paths.
      [ pure currentPath,
        lSearchPaths lang (WorkspaceSearch $ maybeToList mactiveWorkspace),
        lSearchPaths lang (WorkspaceVendoredSearch $ maybeToList mactiveWorkspace),
        lSearchPaths lang (WorkspaceSearch otherWorkspaces),
        lSearchPaths lang (WorkspaceVendoredSearch otherWorkspaces),
        lSearchPaths lang ExternalSearch
      ]

symbolAtPosition :: Language -> Text -> FilePos -> Maybe Text
symbolAtPosition = lSymbolAtPosition

findSymbolDefinitionInPaths :: Language -> Text -> [FilePathKind] -> IO [FilePos]
findSymbolDefinitionInPaths lang symbol paths = do
  positions <- forFn paths $ \(FilePathKind path _) -> do
    msource <- fileText path
    case msource of
      Just source -> do
        -- Do a quick Text within Text check first, before running the full parser.
        -- This helps with performance because for the majority of the files scanned,
        -- the symbol will not be contained in them.
        if symbol `isInfixOf` source
          then return $ map (`filePosWithPath` path) $ lFindSymbolDefinition lang symbol source
          else return []
      _ -> return []
  return $ join positions
  where
    forFn = if length paths > 1 then forConcurrentlyMax else forM

findSymbolDefinition :: Language -> Text -> [IO [FilePathKind]] -> IO [FilePos]
findSymbolDefinition lang symbol searches = do
  case searches of
    [] -> return []
    search : rest -> do
      paths <- search -- Evaluate the file search now.
      positions <- findSymbolDefinitionInPaths lang symbol paths
      case positions of
        [] -> findSymbolDefinition lang symbol rest
        _ -> return positions
