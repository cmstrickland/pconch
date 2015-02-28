(in-package :pconch)


(defun blank-line (line)
  (or (eq 0 (search ";;" line :end2 2))
      (eq 0 (count-if #'alpha-char-p line))))

(defun parse-header (line)
  (format nil "header ~a~%" line))

(defun add-raw-content  (line)
  (format nil "content ~a~%" line))

(defun read-post (open-file)
  (let ((post (make-instance 'post)))
    (loop for line = (read-line open-file nil :EOF)
       with header = nil
       until (eq line :EOF)
       do (cond ((blank-line line) (if header (setf header nil) (setf header t)))
                (header (setf (headers post) (push  (parse-header line) (headers post))))
                (t (setf (content post) (concatenate 'string (content post) line))))) post))


(defclass post ()
  ((headers :accessor headers :initform nil)
   (content :accessor content :initform nil)))


