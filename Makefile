.PHONY: build lint test repl install fmt

build:
	cabal build -j

lint:
	hlint src test app

test:
	cabal test -j --enable-tests

repl:
	cabal repl

install:
	cabal install exe:irk -j --installdir=$${HOME}/.local/bin --overwrite-policy=always $(CABAL_EXTRA_FLAGS)

fmt:
	@find . -type f -name "*.hs" -not -path "*/dist-newstyle/*" -not -path "*/vendor/*" | xargs ormolu -m inplace
