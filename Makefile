GL_GIT=git@gitlab.com:rd--/hsc3-forth.git
GL_HTTP=https://gitlab.com/rd--/hsc3-forth.git

all:
	echo "hsc3-forth"

clean:
	cabal clean
	rm -Rf dist dist-newstyle *~
	(cd cmd; make clean)

get-forth-mode:
	wget http://git.savannah.gnu.org/cgit/gforth.git/plain/gforth.el

push-gl:
	git push $(GL_GIT)

pull-gl:
	git pull $(GL_HTTP)

push-tags:
	git push $(GL_GIT) --tags

update-rd:
	ssh rd@rohandrape.net "(cd sw/hsc3-forth ; git pull $(GL_HTTP))"

push-all:
	make push-gl update-rd
