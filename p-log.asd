(asdf:defsystem :p-log
  :description "Logging functions"
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :depends-on (:cl-ppcre :dc-ds :dc-time)
  :serial t
  :components ((:file "p-log-package")
                (:file "p-log")))
