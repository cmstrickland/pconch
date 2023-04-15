(defpackage #:pconch/tests (:use :cl :fiveam))
(in-package #:pconch/tests)

(def-suite* all-tests)
(def-suite* pconch :in all-tests)
(test test-tests
  (is (= 1 1)))
