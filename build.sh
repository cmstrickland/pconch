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
curl -L http://www.method-combination.net/lisp/files/ironclad.tar.gz  | (cd $QL_LOCAL ; tar xzvf - ) 
curl -L https://common-lisp.net/project/cffi/releases/cffi_latest.tar.gz | (cd $QL_LOCAL ; tar xzvf - )
#sed -e'/pushnew :ironclad-assembly/ s/^/;/g' -i $QL_LOCAL/ironclad-0.45/src/package.lisp 
cp asdf.lisp $HOME
cp *asd *.lisp $QL_LOCAL/pconch/
cp sbcl-compile.lisp $HOME
(cd $HOME ; sbcl --load sbcl-compile.lisp)
