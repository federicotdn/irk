module CLI where

import Control.Monad (forM_)
import qualified Data.Text as T
import Irk (findSymbolDefinition, searchPaths)
import Language (languageFor)
import System.Exit (ExitCode (..), exitWith)
import System.OsPath (unsafeEncodeUtf)
import Utils (ePutStrLn)

runCLI :: FilePath -> FilePath -> String -> IO ()
runCLI active workspace symbol = do
  putStrLn "running search"
  let active' = unsafeEncodeUtf active
  let workspace' = unsafeEncodeUtf workspace
  case languageFor active' of
    Just lang -> do
      putStrLn "get search paths"
      let searches = searchPaths lang (Just active') [workspace']
      putStrLn "search paths done; finding symbols"
      positions <- findSymbolDefinition lang (T.pack symbol) searches
      putStrLn "symbols done"
      forM_ positions $ \pos -> do
        ePutStrLn $ show pos
    Nothing -> do
      ePutStrLn "unknown language"
      exitWith (ExitFailure 1)
