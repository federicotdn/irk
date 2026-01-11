# irk

A tool for finding symbols in codebases, with support for many programming languages*.

irk can be used as a:
- CLI tool: like using [ripgrep](https://github.com/BurntSushi/ripgrep/) to search for symbols, with a bit more code structure awareness.
- LSP server: think [ctags](https://en.wikipedia.org/wiki/Ctags) but without the tags, and queried via LSP.

_*see the [Supported Languages](#supported-languages) section below._

> [!WARNING]
> This project is still at a very early development phase, so it may be missing some features (e.g. language support).

## Features

- Distributed as a single, static binary.
- Search for symbols in codebases without needing to run or set up any project-specific build tools.
- Optionally, search for symbols in 3rd party code, such as `vendor/` for Go, or `.venv` for Python.

## Usage

Install irk by cloning this repo and running `make install`, or by running `make build` and then copying the resulting binary to `$PATH`.

Afterwards, irk can be run as either a CLI tool:

```bash
irk find -l python my_symbol
```

Or as an LSP server listening via stdin:

```bash
irk lsp
```

## Reasoning

The purpose of irk is to mostly work around some LSP servers not behaving correctly in some specific scenarios:
- A project has been cloned but not properly set up - this sometimes trips up some LSP servers.
- A project is too large, and makes LSP servers become sluggish when trying to find a symbol definition.
- The LSP server is directly unable to find a symbol definition, due to the way it was declared (e.g some creative Python code).
- The LSP server does not support finding symbols in vendored code (e.g. Haskell's main LSP implementation).

Other reasons for developing this project include:
- In some cases, the user might be exploring a project written in a language they are not familiar with, and therefore they might not have installed (or know about) the language tooling. In these cases, irk is a "good enough" place to start.
- Developers creating new programming languages could use irk, again, as a "good enough" LSP server while they create the language.
- Once (or if) irk becomes mature enough, users could use it as a fallback server for cases where the main LSP server could not e.g. find a symbol definition. This could be achieved via an LSP server multiplexer like [rassumfrassum](https://github.com/joaotavora/rassumfrassum).

## Performance

irk takes a lot of tricks from ripgrep, and other general ideas, in order to maximize performance:
- Directories are explored in parallel threads, via a work stealing scheme, using a queue to store the remaining directories to be explored.
- Many directories and files are skipped based on a reasonable pre-set `.gitignore`-like set of matching rules.
- Very large files are skipped.
- The remaining files are read using simple read operations, or `mmap`'ed into memory, depending on their size.
- An [algorithm using SIMD/AVX2](http://0x80.pl/notesen/2016-11-28-simd-strfind.html) is used to quickly detect if the symbol occurs _anywhere_ in the target file's contents.
- When a positive match for the above is found, the file's contents are quickly parsed using [megaparsec](https://hackage.haskell.org/package/megaparsec) to check if the symbol appears (for example) as a definition/declaration.
- In LSP mode, the first file to be searched is the one where the definition/declaration query came from. This makes some queries essentially instant.
- Otherwise, first only project files are searched. Afterwards, vendored files are searched. This means that the search radius is initially smaller, and then gets larger (as usually the number of lines of _vendored_ code exceed the number of _project_ lines of code for average-sized projects).
- _Not implemented yet_: searching for _external_ files, e.g. system-level Python/C/etc. installed libraries (assuming source code is available).

## Supported Languages

irk currently supports the following programming languages:

- C (`.c`, `.h`)
- Go (`.go`)
- Haskell (`.hs`, `.hsc`)
- Python (`.py`)
