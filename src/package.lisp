(defpackage #:ql-to-deb
  (:use #:cl)
  (:import-from #:split-sequence #:split-sequence)
  (:import-from #:iolib/os
                #:walk-directory)
  (:import-from #:iolib/pathnames
                #:file-path-namestring))
