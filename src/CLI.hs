module CLI where

import Control.Monad (forM_)
import qualified Data.Text as T
import Irk (findSymbolDefinition, searchPaths)
import Language (languageFor)
import System.Exit (ExitCode (..), exitWith)
import System.OsPath (unsafeEncodeUtf)
import Utils (ePutStrLn)

runCLI :: FilePath -> FilePath -> String -> Bool -> IO ()
runCLI active workspace symbol walkOnly = do
  let active' = unsafeEncodeUtf active
  let workspace' = unsafeEncodeUtf workspace
  case languageFor active' of
    Just lang -> do
      let searches = searchPaths lang (Just active') [workspace']
      if walkOnly
        then do
          s1 <- head searches
          s2 <- searches !! 1
          s3 <- searches !! 2
          ePutStrLn $ "current: " ++ show s1
          ePutStrLn $ "main workspace: " ++ show (length s2)
          -- ePutStrLn $ "main workspace: " ++ show s2
          ePutStrLn $ "main workspace vendored: " ++ show (length s3)
        -- ePutStrLn $ "main workspace vendored: " ++ show s3
        else do
          positions <- findSymbolDefinition lang (T.pack symbol) searches
          forM_ positions $ \pos -> do
            ePutStrLn $ show pos
    Nothing -> do
      ePutStrLn "unknown language"
      exitWith (ExitFailure 1)
