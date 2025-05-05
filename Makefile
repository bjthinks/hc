all: hc testhc

hc:
	cabal build hc

testhc:
	cabal run testhc

clean:
	cabal clean
