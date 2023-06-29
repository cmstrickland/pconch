(in-package pconch)
;; these are dependent on config values so set them here
;; which is safely post-configuration
(defparameter *cache-dir* (cl-fad:merge-pathnames-as-directory *www-dir* #p".cache/"))



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
  (push  (hunchentoot:create-folder-dispatcher-and-handler "/static/styles/" (stylesheet-path) "text/css" ) hunchentoot:*dispatch-table*)
  (swank-loader:init)
  (setf swank::*loopback-interface* "0.0.0.0")
  (let ((ac (serve :port port :prefix root-prefix)))
    (setf (symbol-function 'app)
          #'(lambda (cmd)
              (cond ((equal :start cmd)
                     (hunchentoot:start ac)
                     (swank:create-server :port 4005 :style swank:*communication-style* :dont-close t))
                    ((equal :stop  cmd)
                     (hunchentoot:stop  ac)
                     (swank:stop-server 4005)))))))



(defun boot ()
  "stop the app if it's running, then start it up"
  (let ((*package* (find-package :pconch)))
    (load "local.lisp" :if-does-not-exist nil))
  (defparameter *index-cache* (make-instance 'clache:file-store :directory *cache-dir*))
  (defparameter *cache-version* (dir-mtime *source-dir*))

  (format t "starting ~a ~a ~%" *prefix* *port*)
  (handler-case
      (app :stop)  (condition () nil))
  (setup :root-prefix *prefix* :port *port*)
  (app :start))

(defun main ()
  "entrypoint for the finished app"
  (boot)
  (bordeaux-threads:join-thread
   (find-if
    (lambda (th)
      (search "hunchentoot-listener" (bordeaux-threads:thread-name th)))
    (bordeaux-threads:all-threads))))
