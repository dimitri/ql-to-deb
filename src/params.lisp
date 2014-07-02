;;;
;;; Parameters for the ql-to-deb program.
;;;

(in-package #:ql-to-deb)

(defparameter *version-string* "0.4.0"
  "Our version string...")

(defparameter *verbose* nil
  "Be verbose.")

(defparameter *debian-packages*
  (asdf:system-relative-pathname :ql-to-deb "packages/")
  "Where to find our base packaging")

(defparameter *build-root* "/tmp/ql-to-deb/"
  "Where to build our packages from Quicklisp releases")

(defparameter *logs-root*
  (directory-namestring (uiop:merge-pathnames* "logs/" *build-root*))
  "Where to write the per-package detailed logs")

(defvar *log-stream* nil
  "Currently openend stream where to log all we do.")

(defparameter *archive-directory* "/tmp/ql-to-deb/archives/"
  "Where to fetch the Quicklisp release archives")

(defparameter *ql-props-url*
  "http://beta.quicklisp.org/dist/quicklisp.txt"
  "Current quicklisp properties, including current release file.")

(defparameter *ql-release-property*
  "release-index-url"
  "Property we want to URL of to get at the releases")
