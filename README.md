# irk

A tool for finding symbols in codebases, with support for many programming languages*.

irk can be used as a:
- CLI tool: like using [ripgrep](https://github.com/BurntSushi/ripgrep/) to search for symbols, with a bit more help.
- LSP server: think [ctags](https://en.wikipedia.org/wiki/Ctags) but without the tags.

## Features

- Distributed as a single, static binary.
- Search for symbols in codebases without needing to run or set up any project-specific build tools.
- Optionally, search for symbols in 3rd party code, such as `vendor/` for Go, or `.venv` for Python.

## Supported Languages

irk currently supports the following programming languages:

- C (`.c`, `.h`)
- Go (`.go`)
- Haskell (`.hs`, `.hsc`)
- Python (`.py`)
