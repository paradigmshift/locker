(in-package #:locker-tests)

(define-test split-space-test
  (assert-equal (list "hello" "world") (split-space "hello world")))

(define-test split-equal-test
  (assert-equal (list "file" "default") (split-equal "file=default")))
