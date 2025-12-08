.PHONY: test

build:
	cabal build -j

lint:
	hlint src test app

test:
	cabal test --enable-tests

repl:
	cabal repl

install:
	cabal install exe:irk --installdir=$${HOME}/.local/bin --overwrite-policy=always

fmt:
	@find . -type f -name "*.hs" -not -path "*/dist-newstyle/*" | xargs ormolu -m inplace
