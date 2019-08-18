(defsystem "pconch"
  :description "pconch is pconch by cms"
  :version "0.0.1"
  :author "cms"
  :serial t
  :depends-on ( "prove" #-sbcl"osicat" "cl-who"
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
               (:file "run")))

