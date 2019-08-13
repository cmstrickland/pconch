(defsystem "pconch"
  :description "pconch is pconch by cms"
  :version "0.0.1"
  :author "cms"
  :serial t
  :depends-on ( "prove" "prove-asdf" #-sbcl"osicat" "cl-who"
                        "hunchentoot" "quri" "lquery" "array-utils"
                        "clss"  "trivial-indent" "uiop" "myway"
                        "cl-ppcre" "cl-markdown" "clache" "local-time"
                        "bordeaux-threads" "cl-who"
                )
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
               (:file "run"))
   :in-order-to ((test-op (test-op "pconch/test"))))

(defsystem "pconch/test"
  :description "test suite for pconch"
  :depends-on  ("pconch" "prove" )
  :defsystem-depends-on ("prove-asdf")
  :pathname "tests/"
  :components ((:test-file "test-post"))
  :perform (test-op  (op c)
                    (funcall (intern #.(string :run) :prove) c)))
