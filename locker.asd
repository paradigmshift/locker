;;;; locker.asd

(asdf:defsystem #:locker
  :serial t
  :description "Encrypts and allows access to encrypted files."
  :author "Mozart Reina <mozart@mozartreina.com>"
  :license "FreeBSD License"
  :depends-on (#:salt-n-pepper
               #:ltk)
  :components ((:file "package")
               (:file "common-functions")
               (:file "locker")
               (:file "locker-tests")
               (:file "locker-ltk")))

