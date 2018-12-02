SHELL = /bin/sh
HOME = build/pconch/
QL = ($HOME)/quicklisp
QL_LOCAL = $(QL)/local-projects

.PHONY: clean distclean all quicklisp

$(QL)/setup.lisp:
	sbcl --load "/usr/share/cl-quicklisp/quicklisp.lisp" --eval '(progn (quicklisp-quickstart:install) (quit))'

quicklisp: $(QL)/setup.lisp

quicklisp-local:
	$(mkdir -p $QL_LOCAL)
	touch $QL_LOCAL/banksy

pconch: quicklisp quicklisp-local
	$(mkdir -p $HOME)

all: pconch

clean:
	rm -rf build

distclean: clean
