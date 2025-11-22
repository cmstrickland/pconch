(load "~/asdf.lisp")
(load "~/quicklisp/setup.lisp")

(ql:quickload 'prove-asdf)
(ql:quickload 'pconch)
(in-package :pconch)
(sb-ext:disable-debugger)
(sb-ext:save-lisp-and-die "pconch" :executable t :toplevel 'main)
