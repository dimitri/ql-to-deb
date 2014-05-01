;;;
;;; Parameters for the ql-to-deb program.
;;;

(in-package #:ql-to-deb)

(defparameter *version-string* "0.1.0"
  "Our version string...")

(defparameter *debian-packages*
  (asdf:system-relative-pathname :ql-to-deb "debian/")
  "Where to find our base packaging")

(defparameter *build-root* "/tmp/ql-to-deb/"
  "Where to build our packages from Quicklisp releases")

(defparameter *archive-directory* "/tmp/ql-to-deb/archives/"
  "Where to fetch the Quicklisp release archives")

(defparameter *ql-props-url*
  "http://beta.quicklisp.org/dist/quicklisp.txt"
  "Current quicklisp properties, including current release file.")

(defparameter *ql-release-property*
  "release-index-url"
  "Property we want to URL of to get at the releases")
