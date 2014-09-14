;;;
;;; Update our debian packages with the latest known Quicklisp releases.
;;;

(in-package #:ql-to-deb)

(defparameter *opt-spec*
  `((("help" #\h) :type boolean :documentation "Show usage and exit.")

    (("version" #\V) :type boolean :documentation "Displays version and exit.")

    (("verbose" #\v) :type boolean :documentation "Be verbose.")

    (("fix-bugs" #\F) :type boolean :documentation "Fix packaging bugs.")

    (("status") :type boolean :documentation "Display packages status.")

    (("config" #\c) :type string :initial-value ,*config-filename*
     :documentation "configuration file.")

    (("dir" #\D) :type string :initial-value ,*build-root*
     :documentation "where to build packages.")

    (("logs" #\L) :type string :initial-value ,*logs-root*
     :documentation "where to write detailed logs.")

    (("quicklisp" #\Q) :type string :initial-value ,*ql-props-url*
     :documentation "URL to use to fetch quicklisp.txt main file")))

(defun mkdir-or-die (path &optional (stream *standard-output*))
  "Create a directory at given PATH and exit with an error message when
   that's not possible."
  (handler-case
      (ensure-directories-exist path)
    (condition (e)
      ;; any error here is a panic
      (format stream "PANIC: ~a.~%" e)
      (uiop:quit 5))))

(defun usage (argv &key quit)
  "Show usage then QUIT if asked to."
  (format t "~a [ option ... ] [ package ...]" (first argv))
  (command-line-arguments:show-option-help *opt-spec*)
  (when quit (uiop:quit)))

(defun format-package (source sid-version local-version package release)
  (format t "~:[✗~;✓~] ~a~35t~a~55t~a~70t~a~%"
          (and (string= sid-version local-version)
               (same-version-p package release))
          source
          (or sid-version "")
          local-version
          (ql-version release)))

(defun status (&key
                 (debian-suite "sid")
                 (package-list (list-debian-packages)))
  "Fetch and display current package list status."
  (let ((ql-status     (ql-fetch-current-releases))
        (debian-status (rmadison package-list :suite debian-suite)))
    (format t  "~a~35t~a~55t~a~70t~a~%"
            "  Package"   " sid version"  " local version"  " ql version")
    (format t  "~a~35t~a~55t~a~70t~a~%"
            "-----------" "-------------" "---------------" "------------")
    (loop :for package :in package-list
       :for source  := (deb-source package)
       :for system  := (deb-system package)
       :for local-version := (format nil "~a-~a"
                                     (deb-version package)
                                     (deb-revision package))
       :for sid-version := (gethash source debian-status)
       :for release := (gethash system ql-status)
       :do (format-package source sid-version local-version package release))))

(defun main (argv)
  (let ((args (rest argv)))
    (multiple-value-bind (options packages)
	(handler-case
            (command-line-arguments:process-command-line-options *opt-spec* args)
          (condition (e)
            ;; print out the usage, whatever happens here
            (declare (ignore e))
            (usage argv :quit t)))

      (destructuring-bind (&key help version verbose status fix-bugs
                                config dir logs quicklisp)
	  options

        (when version
	  (format t "ql-to-deb version ~s~%" *version-string*)
          (format t "compiled with ~a ~a~%"
                  (lisp-implementation-type)
                  (lisp-implementation-version)))

	(when help (usage argv))

        (when (or help version) (uiop:quit))

        (when status
          (if packages (status :package-list packages)
              (status))

          (uiop:quit))

        (setf *verbose* verbose)
        (setf *ql-props-url* quicklisp)
        (setf *fix-bugs* fix-bugs)
        (setf *config-filename* config)

        (let ((dir-truename (probe-file dir)))
          (if dir-truename
              (setf *build-root* (directory-namestring dir-truename))
              (setf *build-root* (pathname (mkdir-or-die dir)))))

        (let ((logs-truename (probe-file logs)))
          (if logs-truename
              (setf *logs-root* (directory-namestring logs-truename))
              (setf *logs-root* (mkdir-or-die logs))))

        (handler-case
            (if packages
                (loop :for (package . release)
                   :in (filter-packages-to-update packages)
                   :do (update-package package release))

                ;; by default, try to update them all
                (loop :for (package . release)
                   :in (list-packages-to-update)
                   :do (update-package package release)))

          (condition (c)
            (format t "Fatal: ~a~%" c)
            (uiop:quit 1)))))))
