(in-package :pconch)

(defparameter *route-mapper* (myway:make-mapper))

(defun build-index (params)
  (format t "index ~a" (getf params :category)))

(defun build-entry (params)
  (format t "entry ~a" (getf params :topic)))

(defun build-feed (params)
  (format t "feed:~a" (getf params :category)))

;; (myway:connect *route-mapper* "/"  #'build-index)
;; (myway:connect *route-mapper* "/?:category?/*.rss" #'build-feed)
;; (myway:connect *route-mapper* "/:category/" #'build-index)
;; (myway:connect *route-mapper* "/:category/:topic/?" #'build-entry)

(defun map-routes (routes)
  (let ((mapper (myway:make-mapper)))
    (dolist (r routes mapper)
      (myway:connect mapper (first r) (symbol-function (cadr r))))))

(defparameter *route-mapper*
  (map-routes '(("/" build-index)
                ("/?:category?/*.rss" build-feed)
                ("/:category/" build-index)
                ("/:category/:topic/?" build-entry))))
