module Main where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.List (isInfixOf, isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Options.Applicative
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, (</>))
import System.Info (os)
import System.Process (callCommand)
import Text.Printf (printf)

data RepoInfo = RepoInfo {repoGit :: String, repoCommit :: String}

data Scenario = Scenario
  { scenarioRepo :: String,
    scenarioLang :: String,
    scenarioSymbol :: String
  }

repos :: Map String RepoInfo
repos =
  Map.fromList
    [ ( "k8s",
        RepoInfo
          "https://github.com/kubernetes/kubernetes.git"
          "03e14cc9432975dec161de1e52d7010f9711a913"
      ),
      ( "linux",
        RepoInfo
          "https://github.com/torvalds/linux.git"
          "9d9c1cfec01cdbf24bd9322ed555713a20422115"
      ),
      ( "wikiquote",
        RepoInfo
          "https://github.com/federicotdn/wikiquote.git"
          "2ded998e4f7c5f08e0c66bd34c747a6842a2aedb"
      ),
      ( "ghc",
        RepoInfo
          "https://github.com/ghc/ghc.git"
          "3d6aba778f7628deef0141d9e8effc546c09bac4"
      ),
      ( "rails",
        RepoInfo
          "https://github.com/rails/rails.git"
          "01fb87c25e541ea889263400012bb85e822496f5"
      )
    ]

benchmarks :: Map String Scenario
benchmarks =
  Map.fromList
    [ ("k8s", Scenario "k8s" "go" "genMarkdown"),
      ("k8s-new", Scenario "k8s" "go" "New"),
      ("k8s-vendor", Scenario "k8s" "go" "IgnoreTopFunction"),
      ("k8s-notfound", Scenario "k8s" "go" "ThisSymbolDoesNotExist"),
      ("wq", Scenario "wikiquote" "python" "quote_of_the_day"),
      ("linux", Scenario "linux" "c" "io_madvise"),
      ("ghc", Scenario "ghc" "haskell" "pprWithSourceText"),
      ("rails", Scenario "rails" "ruby" "each_connection_pool")
    ]

data Command = Install | Vendor | Benchmark String Bool Bool

commandParser :: Parser Command
commandParser =
  subparser
    ( command "install" (info (pure Install) (progDesc "Install irk."))
        <> command "vendor" (info (pure Vendor) (progDesc "Vendor dependencies."))
        <> command "benchmark" (info (benchmarkParser <**> helper) (progDesc "Run benchmarks."))
    )

benchmarkParser :: Parser Command
benchmarkParser =
  Benchmark
    <$> argument str (metavar "CASE_NAME")
    <*> switch (long "profile" <> help "Enable profiling.")
    <*> switch (long "rg" <> help "Use ripgrep instead of irk.")

opts :: ParserInfo Command
opts = info (commandParser <**> helper) (fullDesc <> progDesc "Development tools for irk.")

run :: String -> IO ()
run cmd = do
  verbose <- lookupEnv "VERBOSE"
  when (isJust verbose) $ putStrLn $ "running: " ++ cmd
  callCommand cmd

sep :: IO ()
sep = putStrLn $ replicate 80 '-'

install :: Bool -> IO ()
install profile = do
  home <- getHomeDirectory
  let installDir = home </> ".local" </> "bin"
      baseArgs =
        [ "install",
          "exe:irk",
          "-j",
          "--installdir=" ++ installDir,
          "--overwrite-policy=always"
        ]
      profileArgs =
        if profile
          then ["--enable-profiling", "--ghc-options=-fprof-late"]
          else []
      args = baseArgs ++ profileArgs

  result <- try (run $ "cabal " ++ unwords args) :: IO (Either SomeException ())
  case result of
    Left e -> do
      when (os == "mingw32") $
        putStrLn "hint: the .exe file may currently be running, close it and try again."
      putStrLn $ "error: " ++ show e
      exitFailure
    Right () -> return ()

vendor :: IO ()
vendor = do
  run "cabal freeze"

  vendorExists <- doesDirectoryExist "vendor"
  when vendorExists $ removeDirectoryRecursive "vendor"
  createDirectory "vendor"

  contents <- readFile "cabal.project.freeze"
  mapM_ processLine (lines contents)

  removeFile "cabal.project.freeze"
  where
    processLine line
      | "any." `isInfixOf` line = case extractPackage line of
          Just pkg -> run $ "cabal get --destdir=./vendor " ++ pkg ++ " || true"
          Nothing -> return ()
      | otherwise = return ()

    extractPackage line =
      let parts = words line
       in case filter ("any." `isPrefixOf`) parts of
            (p : _) ->
              let name = drop 4 p -- remove "any."
                  version = takeWhile (/= ',') $ drop 2 $ dropWhile (/= '=') line
               in Just $ name ++ "-" ++ filter (/= ' ') version
            [] -> Nothing

benchmark :: String -> Bool -> Bool -> IO ()
benchmark caseName profile useRg = do
  scriptDir <- (</> "etc") <$> getCurrentDirectory
  rootDir <- getCurrentDirectory
  let reposDir = scriptDir </> "repos"

  case Map.lookup caseName benchmarks of
    Nothing -> do
      putStrLn "invalid case; available cases:"
      mapM_ (\c -> putStrLn $ "  " ++ c) (Map.keys benchmarks)
      exitFailure
    Just scenario -> do
      let repoId = scenarioRepo scenario
          language = scenarioLang scenario
          symbol = scenarioSymbol scenario

      case Map.lookup repoId repos of
        Nothing -> do
          putStrLn $ "unknown repo: " ++ repoId
          exitFailure
        Just repoInfo -> do
          let gitUrl = repoGit repoInfo
              commit = repoCommit repoInfo
              repoPath = reposDir </> takeBaseName gitUrl

          putStrLn $ "scenario: " ++ caseName
          putStrLn $ "  git: " ++ gitUrl
          putStrLn $ "  commit: " ++ commit
          putStrLn $ "  language: " ++ language
          putStrLn $ "  symbol: " ++ symbol
          sep

          createDirectoryIfMissing True reposDir

          repoExists <- doesDirectoryExist repoPath
          unless repoExists $
            run $
              "git clone --filter=blob:none " ++ gitUrl ++ " " ++ repoPath

          run $ "git -C " ++ repoPath ++ " checkout --quiet " ++ commit

          setCurrentDirectory rootDir

          case (useRg, profile) of
            (True, _) -> do
              let cmd = "rg -t " ++ language ++ " --word-regexp " ++ symbol ++ " " ++ repoPath ++ " --column --no-heading"
              putStrLn "running rg..."
              run cmd
              sep
              putStrLn "running rg benchmark..."
              run $ "hyperfine -i --warmup 2 '" ++ cmd ++ "'"
            (_, True) -> do
              install True

              profExists <- doesFileExist "irk.prof"
              when profExists $ removeFile "irk.prof"

              sep
              putStrLn "profiling irk..."
              let cmd = "irk find -w " ++ repoPath ++ " -l " ++ language ++ " " ++ symbol ++ " +RTS -pj -N1 -RTS"

              (time, _) <- timeIt $ run cmd
              printf "elapsed time: %.3fs\n" time

              profExists' <- doesFileExist "irk.prof"
              when profExists' $ do
                now <- getCurrentTime
                let timestamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now
                    newname = "irk." ++ timestamp ++ ".prof"
                renameFile "irk.prof" newname
                sep
                putStrLn $ "profile saved to " ++ newname
            (_, _) -> do
              install False

              sep
              putStrLn "running irk..."
              let cmd = "irk find -w " ++ repoPath ++ " -l " ++ language ++ " " ++ symbol

              (time, _) <- timeIt $ run cmd
              printf "elapsed time: %.3fs\n" time

              sep
              putStrLn "running irk benchmark..."
              run $ "hyperfine --warmup 2 '" ++ cmd ++ "'"

timeIt :: IO a -> IO (Double, a)
timeIt io = do
  start <- getCurrentTime
  result <- io
  end <- getCurrentTime
  let diff = diffUTCTime end start
  return (realToFrac diff, result)

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Install -> install False
    Vendor -> vendor
    Benchmark caseName profile useRg -> benchmark caseName profile useRg
