(load "packages.lisp")
(load "handler.lisp")
(load "config.lisp")
(in-package pconch)


(defun serve (&key port prefix)  
  (let ((a (make-instance 'hunchentoot:easy-acceptor :port port)))
    (push (hunchentoot:create-prefix-dispatcher prefix 'handler)
          hunchentoot:*dispatch-table*)
    a))


(defun setup (&key (port 2125) (root-prefix "/"))
  "Create a default acceptor and bind pconch:app to a function that
starts and stops it"
  (let ((ac (serve :port port :prefix root-prefix)))
    (setf (symbol-function 'app)
          #'(lambda (cmd)
              (cond ((equal :start cmd) (hunchentoot:start ac))
                    ((equal :stop  cmd) (hunchentoot:stop  ac)))))))


