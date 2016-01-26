(in-package :pconch)
(defun formatted-date (n)
  "given a universal time return a formatted time string"
  (multiple-value-bind (s m h d M Y d) (decode-universal-time n)
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" Y M d h m)))
