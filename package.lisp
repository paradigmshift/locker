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
           #:hsearch
           #:isearch
           #:item-list
           #:load-contents
           #:display
           #:show-usage-string
           #:find-usage-string
           #:show-all-usage-string))

(defpackage #:locker
  (:use #:cl)
  (:shadowing-import-from #:common-functions
                          #:write-file
                          #:open-file
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
                          #:show-usage-string
                          #:find-usage-string
                          #:show-all-usage-string))

                          

