(in-package :pconch)


(defun templatize-substitute (s c n))

(defmacro templatize (tmpl &rest cmds)
  (loop for (selector op arg) on cmds
     when (equal '= op)  collect `(lquery:$ ,selector (text ,arg)) into tuples
     when (equal '< op)  collect `(lquery:$ ,selector (replace-with ,arg)) into tuples
     finally (return `(progn
                        (lquery:$ (initialize (template-path ,tmpl)))
                        ,@tuples
                        (elt (lquery:$ (serialize)) 0)))))


