prefix=$(HOME)/opt
cmd=hsc3-forth rat-forth

% : %.hs Forth.hs
	ghc -Wall -O2 -fno-warn-orphans --make $<

all: $(cmd)

clean:
	rm -f *.hi *.o $(cmd)

install: $(cmd)
	cp -f $(cmd) $(prefix)/bin

dep:
	cabal install ~/sw/hsc3 ~/sw/hsc3-db ~/sw/hsc3-dot
