(load "packages.lisp")
(in-package pconch)

(defun serve (&key port )
  (setf acceptor (make-instance 'hunchentoot:easy-acceptor :port port)))

(defun app (cmd) cmd)


(defun setup (&key (port 2125) (root-prefix "/") (acceptor nil acceptor-bound-p))
  "Create a default acceptor and bind pconch:app to a function that starts and stops it"
  (let ((acceptor (serve :port port)))
    (setf (symbol-function 'app)
          #'(lambda (cmd)
              (cond ((equal :start cmd) (hunchentoot:start acceptor))
                    ((equal :stop  cmd) (hunchentoot:stop  acceptor)))))))

