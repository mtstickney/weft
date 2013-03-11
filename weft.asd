;;;; weft.asd

(asdf:defsystem #:weft
  :serial t
  :description "Describe weft here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:usocket
               #:bordeaux-threads
               #:trivial-timeout)
  :components ((:file "package")
               (:file "weft")))

