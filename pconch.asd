(defsystem "pconch"
  :description "pconch is pconch by cms"
  :version "0.0.1"
  :author "cms"
  :serial t
  :build-operation "program-op"
  :build-pathname "pconch"
  :entry-point "pconch:main"
  :depends-on (  #-sbcl"osicat" "cl-who"
                        "hunchentoot" "quri" "lquery" "array-utils"
                        "clss"  "trivial-indent" "uiop" "myway"
                        "cl-ppcre" "cl-markdown" "clache" "local-time"
                        "bordeaux-threads" "cl-who"
                 #-(or swank slynk) "swank"
                 )
  :in-order-to ((test-op (test-op "pconch/tests")))
  :components (
               (:file "packages")
               (:file "config")
               (:file "handler")
               (:file "files")
               (:file "post")
               (:file "util")
               (:file "range")
               (:file "routes")
               (:file "dates")
               (:file "markdown")
               (:file "run")))

(defsystem "pconch/tests"
  :description "test suite for pconch"
  :depends-on ("fiveam")
  :pathname "tests"
  :perform (test-op (op c)
                    (symbol-call :fiveam :run! (find-symbol* :all-tests :pconch/tests)))
  :components ((:file "tests")))
