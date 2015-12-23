(in-package :pconch)

;; (myway:connect *route-mapper* "/"  #'build-index)
;; (myway:connect *route-mapper* "/?:category?/*.rss" #'build-feed)
;; (myway:connect *route-mapper* "/:category/" #'build-index)
;; (myway:connect *route-mapper* "/:category/:topic/?" #'build-entry)

(defun map-routes (routes)
  (let ((mapper (myway:make-mapper)))
    (dolist (r routes mapper)
      (myway:connect mapper (first r) (symbol-function (cadr r))))))
