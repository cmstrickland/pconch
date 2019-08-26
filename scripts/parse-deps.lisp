#!/usr/bin/env -S sbcl --script
(defun read-definition (asd)
  (with-open-file (f asd)
    (read f)))

(defun extract-depends-on (definition)
  (let ((idx (position :depends-on definition)))
    (if (< 0 idx (+ 1 (length definition)))
        (elt definition (+ 1 idx)))))

(defun markup-dependency-list (dl)
  (format nil "~{--load-system ~a~%~}" dl))

(defun define-system-loads (asd)
  (markup-dependency-list (extract-depends-on (read-definition asd))))


(format t "~a" (define-system-loads(elt sb-ext:*posix-argv* 1)))
