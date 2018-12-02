#!/bin/bash
export HOME=$PWD/build/pconch
QL_LOCAL=$HOME/quicklisp/local-projects

mkdir -p $HOME
sbcl --load "/usr/share/cl-quicklisp/quicklisp.lisp" --eval '
(progn (quicklisp-quickstart:install)
(quit))
'
mkdir -p $QL_LOCAL
#git clone https://github.com/froydnj/ironclad/ $QL_LOCAL/ironclad
#$(cd $QL_LOCAL/ironclad ; git checkout fe88483bba68eac4db3b48bb4a5a40035965fc84)
mkdir -p $QL_LOCAL/pconch
cp *asd *.lisp $QL_LOCAL/pconch/
cp asdf.lisp $HOME
cp sbcl-compile.lisp $HOME
(cd $HOME ; sbcl --load sbcl-compile.lisp)
