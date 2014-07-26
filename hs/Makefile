prefix=$(HOME)/opt
cmd=hsc3-forth

% : %.hs Forth.hs
	ghc -Wall -fno-warn-orphans --make $<

all: $(cmd)

clean:
	rm -f *.hi *.o $(cmd)

install: $(cmd)
	cp -f $(cmd) $(prefix)/bin
