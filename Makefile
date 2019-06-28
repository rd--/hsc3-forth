clean:
	cabal clean
	rm -Rf dist
	(cd hs; make clean)

push-rd:
	darcs push -a rd@rohandrape.net:sw/hsc3-forth

pull-rd:
	darcs pull -a http://rohandrape.net/sw/hsc3-forth

get-forth-mode:
	wget http://git.savannah.gnu.org/cgit/gforth.git/plain/gforth.el
