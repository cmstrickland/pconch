(defsystem "pconch"
  :description "pconch is cms by cms"
  :version "0.0.1"
  :author "cms"
  :serial t
  :depends-on ("osicat" "cl-who" "hunchentoot" "puri" "lquery" "array-utils"
                "clss" "plump" "trivial-indent" "uiop" "myway"
                "cl-ppcre" "cl-markdown" "clache" "local-time" "bordeaux-threads")
  :components (
               (:file "packages")
               (:file "config")
               (:file "handler")
               (:file "files")
               (:file "post")
               (:file "util")
               (:file "range")
               (:file "plump")
               (:file "routes")
               (:file "dates")
               (:file "markdown")
               (:file "run")))
