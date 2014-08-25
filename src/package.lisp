(defpackage #:ql-to-deb
  (:use #:cl)
  (:import-from #:split-sequence #:split-sequence))

;;;
;;; Package aliasing
;;;
(rename-package 'py-configparser 'py-configparser '(ini))
