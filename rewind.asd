(defpackage :rewind-asd
  (:use :cl :asdf))

(in-package :rewind-asd)

(defsystem rewind
  :license "MIT"
  :author "Kevin Galligan"
  :description "RSS feed generator."
  :depends-on (:hunchentoot :cl-who :lquery :drakma)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "feed")
               (:file "server")
               ))
