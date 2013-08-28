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
               (let ((contents (loop
                                   for line = (read in nil 'eof)
                                   until (eq line 'eof)
                                   collect line)))
                 (string-upcase (code-decode (pass-prompt) (first contents))))
               (let ((ucoded-seq (make-string (file-length in))))
                 (read-sequence ucoded-seq in)
                 (string-upcase ucoded-seq)))))
      `(let ((contents (slurpfile ,fname)))
         (string-upcase (salt-n-pepper:code-decode ,decode (first contents))))))

(defun show (lst)
  (format t "~%~{~a~%~}~%" lst))

(defun show-gui (lst)
  (format nil "~%~{~a~%~}~%" lst))

;;;; Data manipulation for internal representation

(defun split (delimiter collector str)
  "Split the given string on the delimiter(s)"
  (loop for x = 0 then (1+ y)
       as y = (position delimiter str :start x)
       collect (funcall collector x y)
       while y))

(defun split-equal (str)
  "Split on every '='"
  (split #\= #'(lambda (x y)
                 (string-trim " " (subseq str x y)))
         str))

(defun split-header (str)
  "Split on every '*'"
  (split #\* #'(lambda (x y)
                 (string-trim " " (subseq str (if (> x 0) (1- x) x) y)))
         str))

(defun split-newline (str)
  "Split on every newline"
  (split #\Newline #'(lambda (x y)
                       (string-trim " " (subseq str x y)))
         str))

(defun split-space (str)
  "Split on every space"
  (split #\Space #'(lambda (x y)
                     (string-trim " " (subseq str x y)))
         str))

(defun remove-empty-lst (lst)
  (loop for x in lst
       when (> (length (first x)) 0)
       collect x))

(defun remove-empty-entries (lst)
  (remove-if #'zero-length-p lst))

(defun zero-length-p (seq)
  (zerop (length seq)))

(defun sanitize (str)
  "Returns a structured string ready for manipulation by locker"
  (mapcar #'remove-empty-entries
          (remove-empty-lst (mapcar #'split-newline (split-header str)))))

(defun parse-entries (lst)
  "Nests entries under the headers, headers are prefixed with '*'"
  (when (not (null lst))
    (if (char-equal (char (first lst) 0) #\*)
        (cons (intern (first lst)) (list (parse-entries (rest lst))))
        (cons (intern (first lst)) (parse-entries (rest lst))))))

;;;; Query functions

(defun show-category (q lst)
  "Shows all items under the header"
  (let ((query (string-upcase q)))
    (assoc (intern query) lst)))

(defun find-item (query lst)
  "Returns all items matching query"
  (remove-if #'null  (mapcar (lambda (entry)
                               (if (search (string-upcase query) entry)
                                   entry))
                             lst)))

(defun item-list (lst)
  "Returns a list of all items without their headers, list is in the form of '((category (item1 item2...))"
  (map 'list #'string (append-items lst)))

(defun append-items (parsed-entries)
  "Takes a list of a-lists and appends their values into one list"
  (apply #'append (mapcar #'(lambda (alst)
                              (alist-val alst))
                          parsed-entries)))

(defun alist-val (alst)
  "Takes the value of an a-list"
  (second alst))

(defun load-contents (&optional fname pass)
  "Loads the contents in the filename if provided, or in the filename provided by check-rc. Parameters are passed by the GUI, CLI retrieves the posix arguments."
  (cond (fname
         (mapcar #'parse-entries (sanitize (open-file nil fname pass))))
        ((probe-file (first (last cl-user::*posix-argv*)))
         (mapcar #'parse-entries (sanitize (open-file 'cli (first (last cl-user::*posix-argv*)) t))))
        ((check-rc "~/.lockerrc")
         (mapcar #'parse-entries (sanitize (open-file 'cli (check-rc "~/.lockerrc") t))))
        (t (main-usage-string))))

(defun show-usage-string ()
  "show <tag> <filename> -- Unencrypts the file and displays all entries under the tag. Tags are denoted by the '*' followed by a space and the tag name.")

(defun find-usage-string ()
  "find <word/phrase> <filename> -- Unencrypts the file and displays all entries matching the entered word or phrase.")

(defun show-all-usage-string ()
  "show-all <filename> -- Unencrypts the file and displays all tags and entries.")


;;;; RC file
(defun slurpfile (fname)
  "Slurp the contents of a file and return them as symbols"
  (with-open-file (in fname
                      :direction :input)
    (let ((contents (loop
                        for line = (read in nil 'eof)
                        until (eq line 'eof)
                        collect line)))
      contents)))

(defun check-rc (fname)
  "Checks the existence of the rc file and retrieves the value of FILE if it exists"
  (when (directory fname)
    (let ((contents (mapcar #'(lambda (ln)
                       (string ln))
                   (slurpfile fname))))
      (if (equalp (first (split-equal (first contents))) "FILE")
          (second (split-equal (first contents)))
          nil))))

;; (defun split-equal (str)
;;   (split #\= str))

(defun main-usage-string ()
  (format t "~%USAGE~%~%~{~{~<~%~1,80:;~A~> ~}~%~%~}" (mapcar #'split-space (list (edit-usage-string)
                                                                                             (show-usage-string)
                                                                                             (find-usage-string)
                                                                                             (show-all-usage-string)
                                                                                             (encrypt-usage-string)))))

(defun edit-usage-string ()
  "edit <filename> -- Unencrypts the file and passes the contents to an emacs instance. Once editing is done save the file under the same name otherwise the file will be saved unencrypted. A new passphrase for encryption will be asked upon exiting (this happens at the shell, not in the emacs instance).")



(defun encrypt-usage-string ()
  "encrypt <filename> -- Encrypts the file. Be sure to use this only on a plaintext file, if used on an already encrypted file it will encrypt it again making it very hard to return it to a plaintext state.")
