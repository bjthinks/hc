all: fc testfc

fc: *.hs
	ghc --make -O3 fc && touch fc

testfc: *.hs
	ghc --make -O3 testfc && touch testfc

clean:
	rm -f *.hi *.o *~ fc testfc
