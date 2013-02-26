(in-package #:locker-gui)

;;; load functions common to both locker (CLI version) and locker-gui
(load (merge-pathnames "common-functions.lisp"
                       (asdf:system-source-directory :locker)))

(defun load-contents (fname pass)
  (mapcar #'parse-entries (sanitize (open-file fname pass))))

(defun open-file (fname pass)
  (with-open-file (in fname
                      :direction :input)
    (let ((coded-seq (loop
                        for line = (read in nil 'eof)
                        until (eq line 'eof)
                        collect line)))
      (string-upcase (salt-n-pepper:code-decode pass (car coded-seq))))))

(defun show (lst)
  (format nil "~%~{~a~%~}~%" lst))

(defmacro display (contents query &body body)
  `(progn
     (let* ((contents ,contents)
            (query ,query))
       ,@body)))

(defun run-gui ()
  (with-ltk ()
    (let* ((disp-fr (make-instance 'frame))
           (fname-fr (make-instance 'frame))
           (filter-fr (make-instance 'frame))
           (pass-fr (make-instance 'frame))
           (bt-fr (make-instance 'frame))
           (fname-lb (make-instance 'label
                                    :master fname-fr
                                    :text "filename"))
           (filter-lb (make-instance 'label
                                     :master filter-fr
                                     :text "filter"))
           (pass-lb (make-instance 'label
                                   :master pass-fr
                                   :text "pass"))
           (display (make-instance 'text
                                   :master disp-fr))
           (fname-entry (make-instance 'entry
                                       :master fname-fr))
           (filter-entry (make-instance 'entry
                                        :master filter-fr))
           (pass-entry (make-instance 'entry
                                      :master pass-fr
                                      :show "*"))
           (bt1 (make-instance 'button
                               :text "read"
                               :command (lambda ()
                                          (let ((fname (string-trim " " (text fname-entry)))
                                                (pass (text pass-entry))
                                                (s-query (format nil "~{~a~^ ~}" (cdr (split-space (text filter-entry)))))
                                                (filter (car (split-space (text filter-entry)))))
                                            (setf (text display)
                                                  (cond ((string-equal filter "show")
                                                         (display (load-contents fname pass) (concatenate 'string "* " (string-trim " " s-query))
                                                           (show (cadr (hsearch query contents)))))
                                                        ((string-equal filter "show-all")
                                                         (display (load-contents fname pass) nil (format nil "~%~{~{~a~%~{~a~%~}~}~%~}~%" contents)))
                                                        ((string-equal filter "find")
                                                         (display (load-contents fname pass) s-query (show (isearch query (item-list contents)))))
                                                        (t (format nil "~%USAGE~%~%~{~{~<~%~1,80:;~A~> ~}~%~%~}" (mapcar #'split-space (list (show-usage-string)
                                                                                                                                             (find-usage-string)
                                                                                                                                             (show-all-usage-string)))))))))
                               
                               :master bt-fr)))

      (pack fname-fr)
      (pack filter-fr)
      (pack pass-fr)
      (pack bt-fr)
      (pack disp-fr)
      (pack fname-lb :side :left)
      (pack filter-lb :side :left)
      (pack pass-lb :side :left)
      (pack display)
      (pack fname-entry)
      (pack filter-entry)
      (pack pass-entry)
      (pack bt1))))
