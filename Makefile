.PHONY: build lint test repl install fmt

build:
	cabal build -j

lint:
	hlint src test app etc

test:
	cabal test -j --enable-tests

repl:
	cabal repl

install:
	cabal run exe:irk-dev -fdev -- install

fmt:
	@ormolu -m inplace $$(git ls-files '*.hs')
