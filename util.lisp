(in-package :pconch)

(defun remove-prefix (pfx lst)
  "remove as much common prefix from list pfx and lst
 (remove-prefix () '(1 2 3))     -> (1 2 3)
 (remove-prefix '(1) '(1 2 3))   -> (2 3)
 (remove-prefix '(1 2) '(1 2 3)) -> (3)
 (remove-prefix '(1 3) '(1 2 3)) -> (2 3)"
  (if (null pfx) lst
      (progn (if (not (equal (car pfx) (car lst)))
                 lst
                 (remove-prefix (cdr pfx) (cdr lst))))))
