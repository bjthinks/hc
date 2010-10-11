fc: *.hs
	ghc --make -O3 fc

clean:
	rm -f *.hi *.o *~ fc
