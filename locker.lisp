;;;; locker.lisp

(in-package #:locker)

(defun edit-file (fname)
  ;; opens a file in emacs, then encrypts it once emacs exits. remember to save the file!
  (with-open-file (in fname
                      :direction :input)
    (let* ((decoded (open-file 'cli fname t))
           (args (format nil "(progn (create-file-buffer \"~~\/encrypted\") (with-current-buffer \"encrypted\" (insert \"~a\")))" decoded)))
      (sb-ext:run-program "/usr/bin/emacs" (list "--eval" args))
      (write-file (open-file 'cli fname nil) fname t))))

;;;; Command line functions

(defun take-args ()
  "Retrieve query arguments"
  (when (> (length cl-user::*posix-argv*) 2)
    (remove-file (string-trim " " (format nil "~{~a ~}" (subseq cl-user::*posix-argv* 2))))))
  ;; (string-trim " " (format nil "~{~a ~}" (subseq
;; cl-user::*posix-argv* 2 (- (length cl-user::*posix-argv*) 1)))))

(defun remove-file (str)
  "Removes the filename at the end of the argument list if present "
  (let ((args (split-space str)))
    (if (probe-file (first (last args)))
        (first (subseq args 0 (1- (length args))))
        str)))
            
(defun toplevel ()
  (let ((arg1 (nth 1 cl-user::*posix-argv*))
        (arg2 (nth 2 cl-user::*posix-argv*))
        (arg-num (length cl-user::*posix-argv*)))
    (cond ((string-equal "edit" arg1) (edit-file arg2))
          ((string-equal "show" arg1) (display ((contents (load-contents))
                                                (query (concatenate 'string "* " (take-args))))
                                               (force-output (show (cadr (hsearch query contents))))))
          ((string-equal "find" arg1) (display ((contents (load-contents))
                                                (query (take-args)))
                                               (force-output (show (isearch query (item-list contents))))))
          ((string-equal "show-all" arg1) (display ((contents (load-contents))
                                                    (query nil))
                                            (force-output (format t "~%~{~{~a~%~{~a~%~}~}~%~}~%" contents))))
          ((string-equal "encrypt" arg1) (write-file (open-file 'cli arg2 nil) arg2 t))
          (t (main-usage-string)))))

;;;; Testing purposes
(setf cl-user::*posix-argv* '("locker" "show-all" "~/dev/lisp/locker/test.txt"))

(defun dummy-data ()
  (defparameter *test* (load-contents)))

(defun set-posix (&rest args)
  (setf cl-user::*posix-argv* args))
