;;;; package.lisp

(defpackage #:common-functions
  (:use #:cl)
  (:shadowing-import-from #:salt-n-pepper
                          #:code-decode)
  (:export #:write-file
           #:open-file
           #:show
           #:show-gui
           #:split-header
           #:split-newline
           #:remove-empty-lst
           #:remove-empty-entries
           #:zero-length-p
           #:sanitize
           #:parse-entries
           #:split-space
           #:split-equal
           #:hsearch
           #:isearch
           #:item-list
           #:load-contents
           #:display
           #:slurpfile
           #:main-usage-string
           #:show-usage-string
           #:find-usage-string
           #:show-all-usage-string))

(defpackage #:locker
  (:use #:cl)
  (:shadowing-import-from #:common-functions
                          #:write-file
                          #:open-file
                          #:slurpfile
                          #:show
                          #:show
                          #:split-header
                          #:split-newline
                          #:remove-empty-lst
                          #:remove-empty-entries
                          #:zero-length-p
                          #:sanitize
                          #:parse-entries
                          #:split-space
                          #:hsearch
                          #:isearch
                          #:item-list
                          #:load-contents
                          #:display
                          #:main-usage-string
                          #:show-usage-string
                          #:find-usage-string
                          #:show-all-usage-string))

(defpackage #:locker-gui
  (:use #:cl)
  (:shadowing-import-from #:ltk
                          #:with-ltk
                          #:frame
                          #:label
                          #:entry
                          #:button
                          #:text
                          #:pack)
  (:shadowing-import-from #:common-functions
                          #:write-file
                          #:open-file
                          #:show-gui
                          #:split-header
                          #:split-newline
                          #:remove-empty-lst
                          #:remove-empty-entries
                          #:zero-length-p
                          #:sanitize
                          #:parse-entries
                          #:split-space
                          #:hsearch
                          #:isearch
                          #:item-list
                          #:load-contents
                          #:display
                          #:main-usage-string
                          #:show-usage-string
                          #:find-usage-string
                          #:show-all-usage-string))

(defpackage #:locker-tests
  (:use #:cl #:lisp-unit #:common-functions #:locker))
