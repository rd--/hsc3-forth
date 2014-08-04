rhs_dir=$(HOME)/sw/rhs/src
rsc3_dir=$(HOME)/sw/rsc3/src
lisp_dir=$(HOME)/sw/hsc3-forth/lisp

rhs_lib=data/ord.scm data/function.scm data/list.scm data/tree.scm data/tree.scm prelude.scm
rsc3_lib=composite.scm enum.scm env.scm list.scm

rhs:
	(cd $(rhs_dir) ; cat $(rhs_lib) > $(lisp_dir)/rhs.lisp)

rsc3:
	(cd $(rsc3_dir) ; cat $(rsc3_lib) > $(lisp_dir)/rsc3.lisp)
