;;;
;;; Parameters for the ql-to-deb program, specific to the debian packaging
;;; of the tool.
;;;

(in-package #:ql-to-deb)

(defvar *debian-packages*
  (make-pathname :directory "/usr/share/ql-to-deb/packages"))

(defvar *build-root* "/var/cache/ql-to-deb/")
(defvar *logs-root* "/var/logs/ql-to-deb/")
(defvar *archive-directory* "/var/cache/ql-to-deb/archives/")
