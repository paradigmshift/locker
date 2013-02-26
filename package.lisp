;;;; package.lisp

(defpackage #:locker
  (:use #:cl)
  (:shadowing-import-from #:salt-n-pepper
                          #:code-decode))

(defpackage #:locker-gui
  (:use #:cl)
  (:shadowing-import-from #:ltk
                          #:with-ltk
                          #:frame
                          #:label
                          #:entry
                          #:button
                          #:text
                          #:pack))
                          

