all: .cabal-sandbox
	cabal install

.cabal-sandbox:
	cabal sandbox init

yfo: all
	.cabal-sandbox/bin/yfo

clean:
	rm -rf dist
	rm -rf .cabal-sandbox
