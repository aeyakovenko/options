all:
	cabal install

yfo: all
	.cabal-sandbox/bin/yfo
