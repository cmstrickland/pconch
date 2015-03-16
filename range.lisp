;; ranges ( for pagination )
;; they're a list of two integers, representing the start and end of a
;; slice of an index

(in-package :pconch)

(defun compute-range (params)
  (let ((start (cdr (assoc "start" params :test #'equal)))
        (end   (cdr (assoc "end"   params :test #'equal)))
        (a 0)  (z 0))
    (if start
        (setf a (or (parse-integer start :junk-allowed t) 0)))
    (if end
        (setf z (or (parse-integer end :junk-allowed t) 0)))
    (if (< a z)
        (list a z))))

(defun compute-next (range &optional max (step *index-pager*))
  (let ((a (cadr range))
        (z (+ step (cadr range))))
    (if (< a max)
        (progn
          (if (< max z)
              (setf z max))
          (list a z)))))

(defun compute-prev (range &optional (min 0) (step *index-pager*))
  (let ((a (- (car range) step))
        (z (car range)))
    (if (< a min)
        (setf a min))
    (if (< a z)
        (list a z))))
