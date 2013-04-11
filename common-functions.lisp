(in-package #:common-functions)

(defmacro display (((contents-var contents) (query-var query)) &body body)
     `(let* ((,contents-var ,contents)
             (,query-var ,query))
        ,@body))

;;;; File IO

(sb-alien:define-alien-routine getpass sb-alien:c-string (prompt sb-alien:c-string))

(defun pass-prompt ()
   (getpass "passphrase: "))

(defun write-file (txt fname pass)
  (with-open-file (out fname
                       :direction :output
                       :if-exists :supersede)
    (print (code-decode pass txt) out)))

(defmacro open-file (cli fname decode)
  (if cli
      `(let ((decode ,decode))
         (with-open-file (in ,fname
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
      `(with-open-file (in ,fname
                           :direction :input)
         (let ((coded-seq (loop
                             for line = (read in nil 'eof)
                             until (eq line 'eof)
                             collect line)))
           (string-upcase (salt-n-pepper:code-decode ,decode (car coded-seq)))))))

(defun show (lst)
  (format t "~%~{~a~%~}~%" lst))

(defun show-gui (lst)
  (format nil "~%~{~a~%~}~%" lst))

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

;;; splits the string into nested headers and entries
;;; removes empty lists and empty elements in lists
(defun sanitize (str)
  (mapcar #'remove-empty-entries
          (remove-empty-lst (mapcar #'split-newline (split-header str)))))

;;; separates entries into headers (defined by the * at the
;;; beginning of the line) or into items of the previous
;;; header
(defun parse-entries (lst)
  (when (not (null lst))
    (if (char-equal (char (car lst) 0) #\*)
        (cons (intern (car lst)) (list (parse-entries (cdr lst))))
        (cons (intern (car lst)) (parse-entries (cdr lst))))))

(defun split-space (str)
  (loop for x = 0 then (1+ y)
     as y = (position #\Space str :start x)
     collect (string-trim " "(subseq str x y))
     while y))

;;;; Query functions

;;; show all items under header
(defun hsearch (obj lst)
  (let ((query (string-upcase obj)))
    (assoc (intern query) lst)))

;;; show all items matching query
(defun isearch (obj lst)
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

;;; loads and cleans the file, fname and pass used in gui, terminal calls from posix
(defun load-contents (&optional fname pass)
  (if fname
      (mapcar #'parse-entries (sanitize (open-file nil fname pass)))
      (mapcar #'parse-entries (sanitize (open-file 'cli (nth (1- (length cl-user::*posix-argv*))
                                                        cl-user::*posix-argv*)
                                                   t)))))

(defmacro display (((contents-var contents) (query-var query)) &body body)
     `(let* ((,contents-var ,contents)
             (,query-var ,query))
        ,@body))

(defun show-usage-string ()
  "show <tag> <filename> -- Unencrypts the file and displays all entries under the tag. Tags are denoted by the '*' followed by a space and the tag name.")

(defun find-usage-string ()
  "find <word/phrase> <filename> -- Unencrypts the file and displays all entries matching the entered word or phrase.")

(defun show-all-usage-string ()
  "show-all <filename> -- Unencrypts the file and displays all tags and entries.")
