;;;; locker.asd

(asdf:defsystem #:locker
  :serial t
  :description "Describe locker here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:salt-n-pepper)
  :components ((:file "package")
               (:file "locker")))

