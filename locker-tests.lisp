(in-package #:locker-tests)

(with-open-file (out "rc.test" ; test file for check-rc-test
                     :direction :output
                     :if-exists :supersede)
  (print "file=default" out))

(define-test split-space-test
  (assert-equal (list "hello" "world") (split-space "hello world")))

(define-test split-equal-test
  (assert-equal (list "file" "default") (split-equal "file=default")))

(define-test check-rc-test
  (assert-equal "default" (check-rc "rc.test")))
