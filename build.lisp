(ql:quickload "locker")
(in-package :locker)
(sb-ext:save-lisp-and-die "locker" :toplevel #'toplevel :executable t)
