;;;
;;; Parameters for the ql-to-deb program.
;;;

(in-package #:ql-to-deb)

(defparameter *version-string* "0.7.0"
  "Our version string...")

(defparameter *verbose* nil
  "Be verbose.")

(defparameter *on-error-stop* nil
  "As soon as a build fails, stop.")

(defvar *config-filename*
  (asdf:system-relative-pathname :ql-to-deb "ql-to-deb.ini")
  "Configuration file with per-package hooks.")

(defvar *changes-filename* ".ql-to-deb.changes")

(defvar *changes-pathname*
  (uiop:merge-pathnames* *changes-filename* (user-homedir-pathname))
  "Cache the list of just built .changes.")

(defvar *debian-packages*
  (asdf:system-relative-pathname :ql-to-deb "packages/")
  "Where to find our base packaging")

(defvar *build-root* "/tmp/ql-to-deb/"
  "Where to build our packages from Quicklisp releases")

(defvar *logs-root*
  (directory-namestring (uiop:merge-pathnames* "logs/" *build-root*))
  "Where to write the per-package detailed logs")

(defvar *log-stream* nil
  "Currently openend stream where to log all we do.")

(defvar *archive-directory* "/tmp/ql-to-deb/archives/"
  "Where to fetch the Quicklisp release archives")

(defparameter *ql-props-url*
  "http://beta.quicklisp.org/dist/quicklisp.txt"
  "Current quicklisp properties, including current release file.")

(defparameter *ql-release-property*
  "release-index-url"
  "Property we want to URL of to get at the releases")

(defparameter *fix-bugs* nil
  "When true, rebuild debian package for same Quicklisp release.")


;;;
;;; Handle the configuration
;;;
(defun expand-user-homedir-pathname (namestring-or-pathname)
  "Expand NAMESTRING replacing leading ~ with (user-homedir-pathname)"
  (let ((namestring (typecase namestring-or-pathname
                      (string    namestring-or-pathname)
                      (pathname (uiop:native-namestring namestring-or-pathname)))))
    (cond ((or (string= "~" namestring) (string= "~/" namestring))
           (user-homedir-pathname))

          ((and (<= 2 (length namestring))
                (char= #\~ (aref namestring 0))
                (char= #\/ (aref namestring 1)))
           (uiop:merge-pathnames* (uiop:parse-unix-namestring (subseq namestring 2))
                                  (user-homedir-pathname)))

          (t
           (uiop:parse-unix-namestring namestring)))))

(defun set-config-filename (namestring)
  (setf *config-filename* (expand-user-homedir-pathname namestring)))

(defun get-package-hook (package-name hook-name
                         &optional
                           (filename
                            (expand-user-homedir-pathname *config-filename*)))
  "Read the INI configuration file at *config-filename* and return the user
   hook for HOOK for given PACKAGE name, when there's one that exists."
  (when (probe-file filename)
    (let* ((ini  (ini:make-config))
           (conf (ini:read-files ini (list filename))))

      (when (and (ini:has-section-p conf package-name)
                 (ini:has-option-p conf package-name hook-name))
        (ini:get-option conf package-name hook-name)))))
