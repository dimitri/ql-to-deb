;;;
;;; Update our debian packages with the latest known Quicklisp releases.
;;;

(in-package #:ql-to-deb)

(defparameter *opt-spec*
  `((("help" #\h) :type boolean :documentation "Show usage and exit.")

    (("version" #\V) :type boolean :documentation "Displays version and exit.")

    (("quiet"   #\q) :type boolean :documentation "Be quiet")
    (("verbose" #\v) :type boolean :documentation "Be verbose")
    (("debug"   #\d) :type boolean :documentation "Display debug level information.")

    (("dir" #\D) :type string :initial-value ,*build-root*
     :documentation "where to build packages.")

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

(defun main (argv)
  (let ((args (rest argv)))
    (multiple-value-bind (options packages)
	(handler-case
            (command-line-arguments:process-command-line-options *opt-spec* args)
          (condition (e)
            ;; print out the usage, whatever happens here
            (declare (ignore e))
            (usage argv :quit t)))

      (destructuring-bind (&key help version quiet verbose debug dir quicklisp)
	  options

        (declare (ignore quiet verbose debug))

        (setf *ql-props-url* quicklisp)

        (let ((dir-truename (probe-file dir)))
          (if dir-truename
              (setf *build-root* (directory-namestring dir-truename))
              (mkdir-or-die dir)))

        (when version
	  (format t "ql-to-deb version ~s~%" *version-string*)
          (format t "compiled with ~a ~a~%"
                  (lisp-implementation-type)
                  (lisp-implementation-version)))

	(when help
          (usage argv))

        (when (or help version) (uiop:quit))

        (handler-case
            (if packages
                (loop :for package :in packages
                   :do (maybe-update-package package))

                ;; by default, try to update them all
                (loop :for (package . release) :in (list-packages-to-update)
                   :do (update-package package release)))

          (condition (c)
            (format t "Fatal: ~a~%" c)
            (uiop:quit 1)))))))
