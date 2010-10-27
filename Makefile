all: hc testhc

hc: *.hs
	ghc --make hc && touch hc

testhc: *.hs
	ghc --make testhc && touch testhc

clean:
	rm -f *.hi *.o *~ hc testhc
