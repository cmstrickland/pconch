(in-package :pconch)


(defun blank-line (line)
  (or (eq 0 (search ";;" line :end2 2))
      (eq 0 (count-if #'alpha-char-p line))))

(defun parse-header (line)
  (format t "header ~a~%" line))

(defun add-raw-content  (line)
  (format t "content ~a~%" line))

(defun read-post (open-file)
  (loop for line = (read-line open-file nil :EOF)
     with header = nil
     until (eq line :EOF)
     do (cond ((blank-line line) (if header (setf header nil) (setf header t)))
              (header (parse-header line))
              (t (add-raw-content line)))))

