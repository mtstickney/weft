;;;; weft.asd

(asdf:defsystem #:weft
  :serial t
  :description "A TCP server framework, like Hunchentoot for TCP."
  :author "Matthew Stickney <mtstickney@gmail.com>"
  :license "MIT"
  :depends-on (#:usocket
               #:bordeaux-threads
               #:trivial-timeout)
  :components ((:file "package")
               (:file "weft")))

