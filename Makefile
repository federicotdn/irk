.PHONY: build lint test repl install fmt

build:
	cabal build -j

lint:
	hlint src test app

test:
	cabal test -j --enable-tests

repl:
	cabal repl

fmt:
	@ormolu -m inplace $$(git ls-files '*.hs')
