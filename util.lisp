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


(defun split-string (str &optional (ch #\Space))
  "split a string on first incidence of character
 return a list of two parts, or nil if the delimiter was 
not found"
  (let ((i (position ch str)))
    (if i (cons (subseq str 0 i) (subseq str (incf i))))))


(defun kvpair (lst)
  (cons (intern (string-upcase (car lst)) :keyword) (cdr lst)))
