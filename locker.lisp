
;;;; locker.lisp

(in-package #:locker)

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

;;;; Data manipulation for internal representation

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

(defun remove-empty-lst (lst)
  (loop for x in lst
       when (> (length (car x)) 0)
       collect x))

(defun remove-empty-entries (lst)
  (remove-if #'zero-length-p lst))

(defun zero-length-p (seq)
  (zerop (length seq)))

(defun sanitize (str)
  ;; splits the string into nested headers and entries
  ;; removes empty lists and empty elements in lists
  (mapcar #'remove-empty-entries
          (remove-empty-lst (mapcar #'split-newline (split-header str)))))

(defun parse-entries (lst)
  (when (not (null lst))
    (if (char-equal (char (car lst) 0) #\*)
        (cons (intern (car lst)) (list (parse-entries (cdr lst))))
        (cons (intern (car lst)) (parse-entries (cdr lst))))))

(defun load-contents ()
  (mapcar #'parse-entries (sanitize (open-file (nth (1- (length cl-user::*posix-argv*))
                                                    cl-user::*posix-argv*)
                                               t))))

;;;; Query functions

(defun hsearch (obj lst)
  ;; header search, show all entries under header
  (let ((query (string-upcase obj)))
    (assoc (intern query) lst)))

(defun isearch (obj lst)
  ;; item search, show all items matching query
  (let ((results nil))
    (mapcar (lambda (el)
              (if (search (string-upcase obj) el)
                  (push el results)))
            lst)
    results))

(defun item-list (mainlst)
  (let ((item-lst nil))
    (mapcar (lambda (alst)
              (mapcar (lambda (el)
                        (push (string el) item-lst))
                      (cadr alst)))
            mainlst)
    item-lst))

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
          ((string-equal "show" arg1) (display (load-contents)
                                          (concatenate 'string "* " (take-args))
                                        (force-output (show (cadr (hsearch query contents))))))
          ((string-equal "find" arg1) (display (load-contents)
                                          (take-args)
                                        (force-output (show (isearch query (item-list contents))))))
          ((string-equal "show-all" arg1) (display (load-contents)
                                              nil
                                            (format "~a" contents)))
          ((string-equal "encrypt" arg1) (write-file (open-file arg2 nil) arg2 t))
          (t (format t "USAGE ~% edit <filename> -- unencrypts the file, passes the contents to an emacs instance. Once editing is done please remember to save the file under the same name, otherwise the changes will be saved in plaintext. File will be encrypted after exiting.")))))


;;;; Testing purposes
(defun dummy-data ()
  (defparameter *test* (load-contents)))
