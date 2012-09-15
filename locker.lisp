
;;;; locker.lisp

(in-package #:locker)

;;; "locker" goes here. Hacks and glory await!

;;; needs prompt for passphrase
;;; if txt includes lisp code it must be wrapped in a format form
(defun pass-prompt ()
  (write-to-string (read (force-output (format t "passphrase: ")))))

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

(defun split-header (str)
  (loop for x = 0 then (1+ y)
       as y = (position #\* str :start x)
       collect (subseq str (if (> x 0) (1- x) x) y)
       while y))

(defun split-newline (str)
  (loop for x = 0 then (1+ y)
       as y = (position #\Newline str :start x)
       collect (string-trim " "(subseq str x y))
       while y))

(defun split-space (str)
  (loop for x = 0 then (1+ y)
       as y = (position #\Space str :start x)
       collect (string-trim " "(subseq str x y))
       while y))

(defun sanitize (str)
  ;; splits the string into nested headers and entries
  ;; removes empty lists and empty elements in lists
  (mapcar #'remove-empty-entries
          (remove-empty-lst (mapcar #'split-newline (split-header str)))))

(defun zero-length-p (seq)
  (zerop (length seq)))

(defun remove-empty-lst (lst)
  (loop for x in lst
       when (> (length (car x)) 0)
       collect x))

(defun remove-empty-entries (lst)
  (remove-if #'zero-length-p lst))

(defun parse-entries (lst)
  (when (not (null lst))
    (if (char-equal (char (car lst) 0) #\*)
        (cons (intern (car lst)) (list (parse-entries (cdr lst))))
        (cons (intern (car lst)) (parse-entries (cdr lst))))))

(defun access (obj lst)
  (let ((query (string-upcase obj)))
    (assoc (intern query) lst)))

(defun show (obj lst)
  (format t "~a~%~{~a~%~}~%" (car (access obj lst))
          (cadr (access obj lst))))

(defun edit-file (fname)
  ;; opens a file in emacs, then encrypts it once emacs exits. remember to save the file!
  (with-open-file (in fname
                      :direction :input)
    (let* ((decoded (open-file fname t))
           (args (format nil "(progn (create-file-buffer \"~~\/encrypted\") (with-current-buffer \"encrypted\" (insert \"~a\")))" decoded)))
      (sb-ext:run-program "/usr/bin/emacs" (list "--eval" args))
      (write-file (open-file fname nil) fname t))))

(defun toplevel ()
  (let ((arg1 (nth 1 cl-user::*posix-argv*))
        (arg2 (nth 2 cl-user::*posix-argv*))
        (arg-num (length cl-user::*posix-argv*)))
    (cond ((string-equal "edit" arg1) (edit-file arg2))
          ((string-equal "show" arg1) (progn
                                        (let ((contents (mapcar #'parse-entries (sanitize (open-file (nth (1- arg-num) cl-user::*posix-argv*) t)))))
                                               (force-output (show (concatenate 'string "* " (string-trim " " (format nil "~{~a ~}" (subseq cl-user::*posix-argv* 2 (- arg-num 1)))))
                                                                   contents)))))
          ((string-equal "encrypt" arg1) (write-file (open-file arg2 nil) arg2 t))
          (t (format t "USAGE ~% edit <filename> -- unencrypts the file, passes the contents to an emacs instance. Once editing is done please remember to save the file under the same name, otherwise the changes will be saved in plaintext. File will be encrypted after exiting.")))))

(defun create-arg (lst)
  (format nil "~{~a~}" lst))
