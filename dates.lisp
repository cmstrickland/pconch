(in-package :pconch)
(defun formatted-date (n)
  "given a universal time return a formatted time string"
  (multiple-value-bind (s mi h d M Y ) (decode-universal-time n)
    (list (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" Y M d h mi))))

(defun rfc-formatted-datetime (n)
  "given a universal time return RFC 822 formatted date time string"
  (local-time:format-timestring nil n :format local-time:+rfc-1123-format+))

(defun iso-formatted-datetime (n)
  "given a universal time return ISO 8601 formatted date time string"
  (local-time:format-timestring nil n :format local-time:+iso-8601-format+))

(defun parse-pconch-datetime (n)
  "look for YYYY-MM-dd HH:MI dates which is what we use in pconch"
  ;; append a zero seconds component so parse-timestring is happy
  (let* ((ts (if (eql 16 (length n))
                (concatenate 'string n ":00")
                n)))
    (local-time:parse-timestring ts :date-time-separator #\Space)))
