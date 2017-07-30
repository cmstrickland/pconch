(in-package :pconch)

(defun remove-prefix (pfx lst)
  "remove as much common prefix from list pfx and lst
 ...(remove-prefix () '(1 2 3))     -> (1 2 3)
 ...(remove-prefix '(1) '(1 2 3))   -> (2 3)
 ...(remove-prefix '(1 2) '(1 2 3)) -> (3)
 ...(remove-prefix '(1 3) '(1 2 3)) -> (2 3)"
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
        (mapcar  (lambda (s) (string-trim '(#\Space) s))
                 (cdr lst))))

(defun blank-line (line)
  "true if the string provided represents a blank line"
  (or (eq 0 (count-if (lambda (c) (or (digit-char-p c) (alpha-char-p c))) line))
      (eq 0 (search ";;" line :end2 2))))


(defun csv-list (str)
  "split a string of comma separated values into a list"
  (loop for c across str
     with word = (make-array 32
                             :fill-pointer 0
                             :adjustable t
                             :element-type 'character)
     with wordlist
     counting c into pos
     do (unless (eq c #\,) (vector-push-extend c word))
     do (if (or (eq c #\,) (eq pos (length str)))
            (progn
              (push (string-trim '(#\Space) word) wordlist)
              (setf (fill-pointer word) 0)))
     finally (return (reverse wordlist))))

(defun empty-subject (thing)
  (or (not thing)
      (equal "" thing)
      (equal "index" thing)))

(defun truncate-range (range limit)
  (if (> (cadr range) limit)
      (list (car range) limit)
      range))

(defun string-ends-with (str ch)
  (and str (eq ch (elt str (- (length str) 1)))))

(defun append-line (thing line)
	     (setf thing (concatenate 'string thing
				      (string #\newline)
				      line)))
(defun dir-mtime (pathstring)
  (osicat-posix:stat-mtime (osicat-posix:stat (car (uiop:directory* pathstring)))))

(defun subdirs (pathstring)
  ;; needs to make sure merge pathname treats pathstring as a directory
  (let ((pattern (uiop:merge-pathnames* pathstring"*")))
    (remove-if-not #'uiop:directory-pathname-p (uiop:directory* pattern))))

(defun cache-version ()
    (if (eql *cache-version* (dir-mtime *source-dir*))
	*cache-version*
	(progn (clache:clear-cache *index-cache*)
	       (setf *cache-version* (dir-mtime *source-dir*)))))

(defun cache-key (version resource params)
  (format nil "~a-~a-~a" version resource params))
