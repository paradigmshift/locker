;;;; locker.lisp

(in-package #:locker)

;;; load functions common to both locker (CLI version) and locker-gui
(load (merge-pathnames "common-functions.lisp"
                       (asdf:system-source-directory :locker)))

;;;; File IO

(sb-alien:define-alien-routine getpass sb-alien:c-string (prompt sb-alien:c-string))

(defun pass-prompt ()
   (getpass "passphrase: "))

(defun write-file (txt fname encode)
  (with-open-file (out fname
                       :direction :output
                       :if-exists :supersede)
      (if encode
          (print (code-decode (pass-prompt) txt) out)
          (print txt out))))

(defun open-file (fname decode)
  (with-open-file (in fname
                      :direction :input)
    (if decode
        (let ((coded-seq (loop
                            for line = (read in nil 'eof)
                            until (eq line 'eof)
                            collect line)))
          (string-upcase (code-decode (pass-prompt) (car coded-seq))))
        (let ((ucoded-seq (make-string (file-length in))))
          (read-sequence ucoded-seq in)
          (string-upcase ucoded-seq)))))

(defun edit-file (fname)
  ;; opens a file in emacs, then encrypts it once emacs exits. remember to save the file!
  (with-open-file (in fname
                      :direction :input)
    (let* ((decoded (open-file fname t))
           (args (format nil "(progn (create-file-buffer \"~~\/encrypted\") (with-current-buffer \"encrypted\" (insert \"~a\")))" decoded)))
      (sb-ext:run-program "/usr/bin/emacs" (list "--eval" args))
      (write-file (open-file fname nil) fname t))))

(defun load-contents ()
  (mapcar #'parse-entries (sanitize (open-file (nth (1- (length cl-user::*posix-argv*))
                                                    cl-user::*posix-argv*)
                                               t))))

(defun show (lst)
  (format t "~%~{~a~%~}~%" lst))

;;;; Command line functions

(defun take-args ()
  (string-trim " " (format nil "~{~a ~}" (subseq cl-user::*posix-argv* 2 (- (length cl-user::*posix-argv*) 1)))))

(defmacro display (contents query &body body)
  `(progn
     (let* ((contents ,contents)
            (query ,query))
       ,@body)))
            
(defun toplevel ()
  (let ((arg1 (nth 1 cl-user::*posix-argv*))
        (arg2 (nth 2 cl-user::*posix-argv*))
        (arg-num (length cl-user::*posix-argv*)))
    (cond ((string-equal "edit" arg1) (edit-file arg2))
          ((string-equal "show" arg1) (display (load-contents) (concatenate 'string "* " (take-args))
                                        (force-output (show (cadr (hsearch query contents))))))
          ((string-equal "find" arg1) (display (load-contents) (take-args)
                                        (force-output (show (isearch query (item-list contents))))))
          ((string-equal "show-all" arg1) (display (load-contents) nil
                                            (format t "~%~{~{~a~%~{~a~%~}~}~%~}~%" contents)))
          ((string-equal "encrypt" arg1) (write-file (open-file arg2 nil) arg2 t))
          (t (format t "~%USAGE~%~%~{~{~<~%~1,80:;~A~> ~}~%~%~}" (mapcar #'split-space (list (edit-usage-string)
                                                                                  (show-usage-string)
                                                                                  (find-usage-string)
                                                                                  (show-all-usage-string)
                                                                                  (encrypt-usage-string))))))))

(defun edit-usage-string ()
  "edit <filename> -- Unencrypts the file and passes the contents to an emacs instance. Once editing is done save the file under the same name otherwise the file will be saved unencrypted. A new passphrase for encryption will be asked upon exiting (this happens at the shell, not in the emacs instance).")



(defun encrypt-usage-string ()
  "encrypt <filename> -- Encrypts the file. Be sure to use this only on a plaintext file, if used on an already encrypted file it will encrypt it again making it very hard to return it to a plaintext state.")


;;;; Testing purposes
(setf cl-user::*posix-argv* '("locker" "show-all" "~/dev/lisp/locker/test.txt"))

(defun dummy-data ()
  (defparameter *test* (load-contents)))
