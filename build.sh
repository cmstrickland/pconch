#!/bin/bash
export HOME=$PWD/build/pconch
QL_LOCAL=$HOME/quicklisp/local-projects
QL=${QL:-"/usr/share/cl-quicklisp"}
mkdir -p $HOME
sbcl --load "${QL}/quicklisp.lisp" --eval '
(progn (quicklisp-quickstart:install)
(quit))
'

SBCL_VERSION=$(sbcl --version | cut -d. -f2)
mkdir -p $QL_LOCAL
mkdir -p $QL_LOCAL/pconch
# older sbcl won't build the newest ironclad
if [ $SBCL_VERSION -lt "4" ]
   then
       curl -L http://www.method-combination.net/lisp/files/ironclad.tar.gz  | (cd $QL_LOCAL ; tar xzvf - ) 
fi

cp asdf.lisp $HOME
cp *asd *.lisp $QL_LOCAL/pconch/
cp sbcl-compile.lisp $HOME
(cd $HOME ; sbcl --load sbcl-compile.lisp)
