prefix=$(HOME)/opt
cmd=hsc3-forth rat-forth

% : %.hs Forth.hs
	ghc -Wall -O2 -fno-warn-orphans --make $<

all: $(cmd)

clean:
	rm -f *.hi *.o $(cmd)

install: $(cmd)
	cp -f $(cmd) $(prefix)/bin
