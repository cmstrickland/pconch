;;(load "~/asdf.lisp")
(load "~/quicklisp/setup.lisp")

;; fix quicklisp dist to something known to be sane
(use-package :ql-dist)
(install-dist "http://beta.quicklisp.org/dist/quicklisp/2018-10-18/distinfo.txt" :replace t :prompt nil)
;;

(ql:quickload 'prove-asdf)
(ql:quickload 'pconch)
(in-package :pconch)
(sb-ext:disable-debugger)
(sb-ext:save-lisp-and-die "pconch" :executable t :toplevel 'main)
