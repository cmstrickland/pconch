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


(defun bisect-string (str &optional (ch #\Space))
  "split a string on first incidence of character
 return a list of two parts, or nil if the delimiter was 
not found. Character defaults to space unless supplied"
  (let ((i (position ch str)))
    (if i (cons (subseq str 0 i) (subseq str (incf i))))))


(defun kvpair (lst)
  "convert a cons cell of two strings into a cons cell where the car
is a keyword and the cdr is whitespace trimmed"
  (cons (intern (string-upcase (car lst)) :keyword)
        (string-trim '(#\Space) (cdr lst))))

(defun blank-line (line)
  "true if the string provided represents a blank line"
  (or (eq 0 (search ";;" line :end2 2))
      (eq 0 (count-if #'alpha-char-p line))))

