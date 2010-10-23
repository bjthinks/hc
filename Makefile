all: fc testfc

fc: *.hs
	ghc --make fc && touch fc

testfc: *.hs
	ghc --make testfc && touch testfc

clean:
	rm -f *.hi *.o *~ fc testfc
