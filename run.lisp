(in-package pconch)
;; these are dependent on config values so set them here
;; which is safely post-configuration
(defparameter *cache-dir* (cl-fad:merge-pathnames-as-directory *www-dir* #p".cache/"))
(defparameter *index-cache* (make-instance 'clache:file-store :directory *cache-dir*))
(defparameter *cache-version* (dir-mtime *source-dir*))



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
  (let ((ac (serve :port port :prefix root-prefix)))
    (setf (symbol-function 'app)
          #'(lambda (cmd)
              (cond ((equal :start cmd) (hunchentoot:start ac))
                    ((equal :stop  cmd) (hunchentoot:stop  ac)))))))



(defun boot ()
  "stop the app if it's running, then start it up"
  (handler-case
      (app :stop)  (condition () nil))
  (setup :root-prefix *prefix* :port *port*)
  (app :start))

(defun main (args)
  (boot)
  (bordeaux-threads:join-thread
   (find-if
    (lambda (th)
      (search "hunchentoot-listener" (bordeaux-threads:thread-name th)))
    (bordeaux-threads:all-threads))))
