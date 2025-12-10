module Main (main) where

import CLI (runCLI)
import Server (createServer, runServer)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdin, stdout)
import Utils (ePutStrLn)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  args <- getArgs
  case args of
    "lsp" : _ -> do
      let srv = createServer ("--verbose" `elem` args)
      runServer srv
    "search" : _ -> do
      runCLI (args !! 1) (args !! 2) (args !! 3) ("--walk-only" `elem` args)
    _ -> do
      ePutStrLn "irk: unknown command"
      exitWith (ExitFailure 1)
