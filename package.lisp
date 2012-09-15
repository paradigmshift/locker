;;;; package.lisp

(defpackage #:locker
  (:use #:cl)
  (:shadowing-import-from #:salt-n-pepper
                          #:code-decode))

