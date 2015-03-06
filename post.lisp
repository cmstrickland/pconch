(in-package :pconch)


(defun parse-header (line)
  (kvpair (bisect-string line #\:)))

(defun add-raw-content  (line)
  (format nil "content ~a~%" line))

(defun read-post (open-file)
  (loop for line = (read-line open-file nil :EOF)
     with in-header = nil
     with post = (make-instance 'post)
     until (eq line :EOF) do
       (cond ((blank-line line)   (if in-header
                                      (setf in-header nil)
                                      (setf in-header t)))
             (in-header             (setf (headers post)
                                       (push  (parse-header line) (headers post))))
             (t                  (setf (content post)
                                       (concatenate 'string (content post) line))))
     finally (return post)))

(defclass post ()
  ((headers :accessor headers :initform nil)
   (content :accessor content :initform nil)))

(defgeneric render (post &optional template))
(defgeneric header (post header))

(defmethod render ((post post) &optional (template "post"))
  (lquery:$ (initialize (template-path template)))
  (lquery:$ "div#content > p" (replace-with (content post)))
  (lquery:$ "h1#post-heading" (text (header post :title)))
  (elt (lquery:$ (serialize)) 0))

(defmethod header ((post post) header)
  (cdr (assoc header (headers post))))
