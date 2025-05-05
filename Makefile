all: hc testhc

hc:
	cabal build

testhc:
	#ghc --make testhc && touch testhc

clean:
	cabal clean
