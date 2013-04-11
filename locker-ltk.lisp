(in-package #:locker-gui)

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
           (encrypt-bt (make-instance 'button
                                      :text "encrypt"
                                      :command (lambda ()
                                                 (let ((fname (string-trim " " (text fname-entry)))
                                                       (pass (text pass-entry))
                                                       (doc (text display)))
                                                   (write-file doc fname pass)))
                                      :master bt-fr))
             
           (read-bt (make-instance 'button
                               :text "read"
                               :command (lambda ()
                                          (let ((fname (string-trim " " (text fname-entry)))
                                                (pass (text pass-entry))
                                                (s-query (format nil "~{~a~^ ~}" (cdr (split-space (text filter-entry)))))
                                                (filter (car (split-space (text filter-entry)))))
                                            (setf (text display)
                                                  (cond ((string-equal filter "show")
                                                         (display ((contents (load-contents fname pass))
                                                                   (query (concatenate 'string "* " (string-trim " " s-query))))
                                                                  (show-gui (cadr (hsearch query contents)))))
                                                        ((string-equal filter "show-all")
                                                         (display ((contents (load-contents fname pass))
                                                                   (query nil))
                                                                  (format nil "~%~{~{~a~%~{~a~%~}~}~%~}~%" contents)))
                                                        ((string-equal filter "find")
                                                         (display ((contents (load-contents fname pass))
                                                                   (query s-query))
                                                                  (show-gui (isearch query (item-list contents)))))
                                                        (t (format nil "~%USAGE~%~%~{~{~<~%~1,80:;~A~> ~}~%~%~}" (mapcar #'split-space (list (show-usage-string)
                                                                                                                                             (find-usage-string)
                                                                                                                                             (show-all-usage-string)))))))))
                               :master bt-fr)))

      (pack fname-fr :ipadx 375 :pady 0)
      (pack filter-fr :ipadx 388 :pady 0)
      (pack pass-fr :ipadx 387 :pady 0)
      (pack bt-fr :ipadx 400 :pady 0)
      (pack disp-fr :ipadx 200 :pady 0)
      (pack display :side :left)
      (pack fname-entry :side :left)
      (pack filter-entry :side :left)
      (pack pass-entry :side :left)
      (pack fname-lb :side :left)
      (pack filter-lb :side :left)
      (pack pass-lb :side :left)
      (pack read-bt :side :left)
      (pack encrypt-bt :side :left))))
