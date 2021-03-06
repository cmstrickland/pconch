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

(defun whitespace-char-p (c)
  (find c'(#\Space #\Tab #\Linefeed #\Return #\Page)))

(defun blank-line (line)
  "true if the string provided represents a blank line"
  (cond
    ((eql 0 (length line)) t)
    ((eql 1 (length line)) (if (whitespace-char-p (elt line 0)) t nil))
    ((equal ";;" (subseq line 0 2)) t)
    (t  (not(some (lambda (c) (not (whitespace-char-p c))) line)))))



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
  (car (sort (mapcar (lambda (f )
                       (osicat-posix:stat-mtime (osicat-posix:stat f)))
                     (uiop:directory-files (uiop:merge-pathnames* pathstring "*")))
             #'>)))


(defun cache-version ()
    (if (eql *cache-version* (dir-mtime *source-dir*))
	*cache-version*
	(progn (clache:clear-cache *index-cache*)
	       (setf *cache-version* (dir-mtime *source-dir*)))))

(defun cache-key (version resource params)
  (format nil "~a-~a-~a" version resource params))
