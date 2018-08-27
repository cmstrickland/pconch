(in-package :pconch)

;; routes that begin with an "@" symbol require authentication
(defun authy-route (r)
  (equal (subseq r 0 1) "@"))

(defun map-routes (routes)
  (let ((mapper (myway:make-mapper)))
    (dolist (r routes mapper)
      (let* ((path (first r))
            (func (cadr r))
            (handler (if (authy-route path)
                         (progn
                           (setf path (subseq path 1))
                           (lambda (p) (with-auth "pconch" func p)))
                         (symbol-function func))))
        (myway:connect mapper path handler)))))
