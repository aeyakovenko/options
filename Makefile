all: .cabal-sandbox
	cabal install

.cabal-sandbox:
	cabal sandbox init

run: all
	.cabal-sandbox/bin/options

clean:
	rm -rf dist
	rm -rf .cabal-sandbox
