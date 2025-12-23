module Main (main) where

import CLI (FindOptions (..), runFind)
import Options.Applicative
  ( Parser,
    argument,
    command,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    progDesc,
    short,
    showDefault,
    str,
    strOption,
    switch,
    value,
    (<**>),
  )
import Server (ServerOptions (..), runServer)

data Options = Options
  { oVerbose :: Bool,
    oCommand :: Command
  }

data Command
  = Server ServerOptions
  | Find FindOptions

serverCommand :: Parser Command
serverCommand = pure $ Server (ServerOptions True)

findCommand :: Parser Command
findCommand =
  Find
    <$> ( FindOptions
            <$> strOption (long "workspace" <> short 'w' <> value "." <> showDefault)
            <*> strOption (long "language" <> short 'l' <> metavar "LANGUAGE")
            <*> argument str (metavar "SYMBOL")
            <*> pure True
        )

parser :: Parser Options
parser =
  Options
    <$> switch (long "verbose" <> short 'v' <> help "Whether to enable verbose output.")
    <*> hsubparser
      ( command "lsp" (info serverCommand (progDesc "Start the irk LSP server."))
          <> command "find" (info findCommand (progDesc "Find a symbol."))
      )

main :: IO ()
main = do
  let fullParser =
        info
          (parser <**> helper)
          ( fullDesc
              <> progDesc "Find symbols in codebases."
              <> header "irk - a tool for finding symbols"
          )
  options <- execParser fullParser
  case oCommand options of
    Server serverOptions -> runServer serverOptions {sVerbose = oVerbose options}
    Find findOptions -> runFind findOptions {fVerbose = oVerbose options}
