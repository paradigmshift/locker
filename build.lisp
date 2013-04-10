(ql:quickload "locker")

;;; build CLI program
(in-package :locker)
(sb-ext:save-lisp-and-die "locker" :toplevel #'toplevel :executable t)
