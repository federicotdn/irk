module CLI (runFind, FindOptions (..)) where

import Control.Monad (forM_)
import qualified Data.Text as T
import Irk (findSymbolDefinition, searchPaths)
import Language (languageByName)
import System.Exit (ExitCode (..), exitWith)
import System.OsString (decodeUtf, unsafeEncodeUtf)
import Types (IrkFile (..), IrkFilePos (..))
import Utils (ePutStrLn, ignoreIOError)

data FindOptions = FindOptions
  { fWorkspace :: FilePath,
    fLanguage :: String,
    fSymbol :: String,
    fVerbose :: Bool
  }

runFind :: FindOptions -> IO ()
runFind options = do
  let workspace = unsafeEncodeUtf (fWorkspace options)
  let language = fLanguage options
  let symbol = T.pack (fSymbol options)
  case languageByName language of
    Just lang -> do
      let searches = searchPaths lang Nothing [workspace]
      positions <- findSymbolDefinition lang symbol searches
      forM_ positions $ \(IrkFilePos f l c) -> do
        result <- ignoreIOError (decodeUtf $ iPath f)
        case result of
          Just decoded -> ePutStrLn $ decoded ++ ":" ++ show (l + 1) ++ ":" ++ show (c + 1)
          Nothing -> pure ()
    Nothing -> do
      ePutStrLn "error: unknown language"
      exitWith (ExitFailure 1)
