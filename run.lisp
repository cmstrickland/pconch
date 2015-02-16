(load "packages.lisp")
(in-package pconch)

(defun serve (&key port )
  (make-instance 'hunchentoot:easy-acceptor :port port))


(defun setup (&key (port 2125) (root-prefix "/"))
  "Create a default acceptor and bind pconch:app to a function that starts and stops it"
  (let ((ac (serve :port port)))
    (setf (symbol-function 'app)
          #'(lambda (cmd)
              (cond ((equal :start cmd) (hunchentoot:start ac))
                    ((equal :stop  cmd) (hunchentoot:stop  ac)))))))

