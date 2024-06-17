prefix=$(HOME)/opt
cmd=hsc3-forth rat-forth

% : %.hs
	ghc -Wall -Wno-x-partial -Wno-incomplete-uni-patterns -fwarn-tabs -O2 -threaded --make $<

all: $(cmd)

clean:
	rm -f *.hi *.o $(cmd)

install: $(cmd)
	cp -f $(cmd) $(prefix)/bin
