;;;
;;; Update our debian packages with the latest known Quicklisp releases.
;;;

(in-package #:ql-to-deb)

(defparameter *commands*
  '(("status"  "Compare versions (debian sid, local, Quicklisp)" status)
    ("check"   "List packages that need building"                check)
    ("update"  "Build either packages or all that need a build"  update)
    ("build"   "Build either packages or all that need a build"  update)
    ("lint"    "Run lintian on just built packages"              lint)
    ("install" "Run dpkg -i on just built packages"              install)
    ("sign"    "Run debsign on just built packages"              sign)
    ("upload"  "Run dput on just built packages"                 upload))
  "Top level commands for the ql-to-deb command line")

(defparameter *opt-spec*
  `((("help" #\h) :type boolean :documentation "Show usage and exit.")

    (("version" #\V) :type boolean :documentation "Displays version and exit.")

    (("verbose" #\v) :type boolean :documentation "Be verbose.")

    (("fix-bugs" #\F) :type boolean :documentation "Fix packaging bugs.")

    (("config" #\c) :type string :initial-value ,*config-filename*
     :documentation "configuration file.")

    (("changes" #\C) :type string :initial-value ,*changes-filename*
     :documentation "ql-to-deb changes file.")

    (("dir" #\D) :type string :initial-value ,*build-root*
     :documentation "where to build packages.")

    (("logs" #\L) :type string :initial-value ,*logs-root*
     :documentation "where to write detailed logs.")

    (("on-error-stop" #\E) :type boolean :documentation "Stop at first error.")

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
  (format t "~a [ option ... ] command [ package ...]" (first argv))

  (format t "~&~%Options:~%")
  (command-line-arguments:show-option-help *opt-spec*)

  ;;
  ;; Usage strings for commands
  ;;
  (format t "~&~%Commands:~%")
  (loop :for (command help fn) :in *commands*
     :do (format t "  ~a~12t~a~%" command help))

  (when quit (uiop:quit)))

(defun process-command-line (&key arguments
                               config changes
                               verbose fix-bugs
                               dir logs quicklisp on-error-stop)
  (setf *verbose* verbose)
  (setf *on-error-stop* on-error-stop)
  (setf *ql-props-url* quicklisp)
  (setf *fix-bugs* fix-bugs)
  (setf *config-filename* config)
  (setf *changes-filename* (uiop:parse-native-namestring changes))

  (let ((dir-truename (probe-file dir)))
    (if dir-truename
        (setf *build-root* (directory-namestring dir-truename))
        (setf *build-root* (pathname (mkdir-or-die dir)))))

  (let ((logs-truename (probe-file logs)))
    (if logs-truename
        (setf *logs-root* (directory-namestring logs-truename))
        (setf *logs-root* (mkdir-or-die logs))))

  (when verbose
    (format t "       config: ~s~%" *config-filename*)
    (format t "      changes: ~s~%" *changes-filename*)
    (format t "     fix-bugs: ~:[false~;true~]~%" *fix-bugs*)
    (format t "on-error-stop: ~:[false~;true~]~%" *on-error-stop*)
    (format t "   build-root: ~a~%" *build-root*)
    (format t "    logs-root: ~a~%" *logs-root*))

  (handler-case
      (destructuring-bind (command &rest packages) arguments
        (let ((cmd (assoc command *commands* :test #'string=)))
          (if cmd
              (destructuring-bind (name help fn) cmd
                (declare (ignore name help))
                (funcall fn :packages packages))
              (error "Unknown command ~s" command))))
    (condition (c)
      (format t "Fatal: ~a~%" c)
      (uiop:quit 1)))

  (uiop:quit))

(defun main (argv)
  (handler-case
      (let ((args (rest argv)))
        (multiple-value-bind (options arguments)
            (command-line-arguments:process-command-line-options *opt-spec* args)
          (destructuring-bind (&key help version verbose fix-bugs
                                    config changes
                                    dir logs quicklisp on-error-stop)
              options
            (when version
              (format t "ql-to-deb version ~s~%" *version-string*)
              (format t "compiled with ~a ~a~%"
                      (lisp-implementation-type)
                      (lisp-implementation-version)))

            (when help (usage argv))

            (when (or help version) (uiop:quit))

            (process-command-line :arguments arguments
                                  :config config
                                  :changes changes
                                  :verbose verbose
                                  :fix-bugs fix-bugs
                                  :dir dir
                                  :logs logs
                                  :quicklisp quicklisp
                                  :on-error-stop on-error-stop))))

    ;; print out the usage, whatever happens here
    (condition (e)
      (declare (ignore e))
      (usage argv :quit t))))
