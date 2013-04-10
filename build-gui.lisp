(ql:quickload "locker")

;;; build gui program
(in-package :locker-gui)
(sb-ext:save-lisp-and-die "locker-gui"  :toplevel #'run-gui :executable t)
