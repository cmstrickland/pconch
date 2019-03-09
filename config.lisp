(in-package :pconch)
(defparameter *base* "https://beatworm.co.uk/")
(defparameter *prefix* "/blog/")
(defparameter *port* 2125)
(defparameter *source-dir* #p"/pconch/posts/")
(defparameter *www-dir* #p"/pconch/html/")
(defparameter *template-dir* #p "/usr/share/pconch/templates/") ;; needs override
(defparameter *index-pager* 20)
(defvar *last-request* nil)
(defvar *script-name* nil)
;; local overrides can go in here, for site configuration
(load "local.lisp" :if-does-not-exist nil)
