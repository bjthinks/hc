all: fc testfc

fc: *.hs
	ghc --make -O3 fc

testfc: *.hs
	ghc --make -O3 testfc

clean:
	rm -f *.hi *.o *~ fc testfc
