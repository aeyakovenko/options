all: .cabal-sandbox
	cabal install

.cabal-sandbox:
	cabal sandbox init

clean:
	rm -rf dist
	rm -rf .cabal-sandbox
