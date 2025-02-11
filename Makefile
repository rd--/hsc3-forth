all:
	echo "hsc3-forth"

clean:
	cabal clean
	rm -Rf dist dist-newstyle *~
	(cd cmd; make clean)

get-forth-mode:
	wget http://git.savannah.gnu.org/cgit/gforth.git/plain/gforth.el

push-all:
	r.gitlab-push.sh hsc3-forth
	r.github-push.sh hsc3-forth

push-rd:
	git push ssh://rd@rohandrape.net/~rd/sw/hsc3-forth master

indent:
	fourmolu -i Language cmd

doctest:
	doctest -Wno-x-partial -Wno-incomplete-uni-patterns Language
