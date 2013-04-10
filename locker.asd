;;;; locker.asd

(asdf:defsystem #:locker
  :serial t
  :description "Encrypts and allows access to encrypted files."
  :author "Mozart Reina <mozart@mozartreina.com>"
  :license "Specify license here"
  :depends-on (#:salt-n-pepper
               #:ltk)
  :components ((:file "package")
               (:file "locker")
               (:file "common-functions")
               (:file "locker-ltk")))

