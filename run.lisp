(load "packages.lisp")
(load "handler.lisp")
(load "config.lisp")
(load "files.lisp")
(load "post.lisp")
(load "util.lisp")

(in-package pconch)


(defun serve (&key port prefix)  
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :port port)))
    (push (hunchentoot:create-prefix-dispatcher prefix 'handler)
          hunchentoot:*dispatch-table*)
    acceptor))


;; build an 'app defun that takes :start and :stop args
;; to start/stop the service
(defun setup (&key (port 2125) (root-prefix "/"))
  "Create a default acceptor and bind pconch:app to a function that
starts and stops it"
  (let ((ac (serve :port port :prefix root-prefix)))
    (setf (symbol-function 'app)
          #'(lambda (cmd)
              (cond ((equal :start cmd) (hunchentoot:start ac))
                    ((equal :stop  cmd) (hunchentoot:stop  ac)))))))



(defun boot ()
  "stop the app if it's running, then start it up"
  (handler-case 
      (app :stop)  (ccl::undefined-function-call () nil)) 
  (setup :root-prefix *prefix* :port *port*)
  (app :start))

;; start it up
(boot) 
