#!/bin/bash
export HOME=$PWD/build/pconch
QL_LOCAL=$HOME/quicklisp/local-projects

mkdir -p $HOME
sbcl --load "/usr/share/cl-quicklisp/quicklisp.lisp" --eval '
(progn (quicklisp-quickstart:install)
(quit))
'
mkdir -p $QL_LOCAL
mkdir -p $QL_LOCAL/pconch
cp *asd *.lisp $QL_LOCAL/pconch/
cp asdf.lisp $HOME
cp sbcl-compile.lisp $HOME
(cd $HOME ; sbcl --load sbcl-compile.lisp)
