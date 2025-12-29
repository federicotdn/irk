module CLI
  ( runFind,
    FindOptions (..),
  )
where

import Control.Exception (try)
import Control.Monad (forM_)
import qualified Data.Text as T
import Irk (findSymbolDefinition, searchPaths)
import Language (languageByName)
import System.Exit (ExitCode (..), exitWith)
import System.OsPath (OsPath, normalise)
import System.OsPath.Encoding (EncodingException)
import System.OsString (decodeUtf, encodeUtf)
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
  eworkspace <- try $ encodeUtf (fWorkspace options) :: IO (Either EncodingException OsPath)
  let language = fLanguage options
  let symbol = T.pack (fSymbol options)
  let mlanguage = languageByName language
  case (mlanguage, eworkspace) of
    (Just lang, Right workspace) -> do
      let searches = searchPaths lang Nothing [workspace]
      positions <- findSymbolDefinition lang symbol searches
      forM_ positions $ \(IrkFilePos f l c) -> do
        result <- ignoreIOError (decodeUtf . normalise $ iPath f)
        case result of
          Just decoded -> ePutStrLn $ decoded ++ ":" ++ show (l + 1) ++ ":" ++ show (c + 1)
          Nothing -> pure ()
    (Just _, Left _) -> do
      ePutStrLn "error: unable to encode workspace path"
      exitWith (ExitFailure 1)
    (Nothing, _) -> do
      ePutStrLn "error: unknown language"
      exitWith (ExitFailure 1)
