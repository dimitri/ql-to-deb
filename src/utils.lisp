;;;
;;; Random bits that didn't fit anywhere else really
;;;
(in-package #:ql-to-deb)

(defun last-directory (pathname)
  "Return the last directory component of PATHNAME."
  (multiple-value-bind (flag path-list last-component file-namestring-p)
      (uiop:split-unix-namestring-directory-components (namestring pathname))
    (declare (ignore flag last-component file-namestring-p))
    (car (last path-list))))

(defun make-symlink (source target)
  "uiop doesn't provide a direct way to do that..."
  (run-command `("ln" "-s"
                      ,(uiop:native-namestring source)
                      ,(uiop:native-namestring target))
               (uiop:pathname-directory-pathname source)))

(defun unpack-archive (archive-pathname)
  "Unpack already fetched RELEASE in *BUILD-ROOT*"
  (ensure-directories-exist (directory-namestring *build-root*))

  (let* ((tar `("tar" "xzf" ,(uiop:native-namestring archive-pathname))))
    (run-command tar *build-root*)))

(defun dpkg-architecture ()
  "Return the result of `dpkg --print-architecture`."
  (let ((output
         (uiop:run-program '("dpkg" "--print-architecture") :output :string)))
   (with-input-from-string (s output)
     (read-line s))))

(defun lintian (changes-filename)
  "Print whatever the lintian command outputs."
  (uiop:run-program `("lintian" ,changes-filename)
                    :output :interactive
                    :error-output :interactive
                    :ignore-error-status t))

(defun debsign (changes-filename)
  "Call `debsign` on given CHANGES-FILENAME."
  (uiop:run-program `("debsign" ,changes-filename)
                    :output :interactive
                    :error-output :interactive
                    :ignore-error-status t))

(defun dput (changes-filename)
  "Call `dput` on given CHANGES-FILENAME."
  (uiop:run-program `("dput" ,changes-filename)
                    :output :interactive
                    :error-output :interactive
                    :ignore-error-status t))

(defun ensure-list (maybe-list)
  (typecase maybe-list
    (list maybe-list)
    (t    (list maybe-list))))

(defun run-command (command cwd
                    &key
                      ignore-error-status
                      (log-stream *log-stream*))
  "Run specified COMMAND (a list of strings) within CWD."
  (flet ((format-command (stream command)
           (format stream "~{~a~^ ~}~%" (mapcar (lambda (arg)
                                                  (if (find #\Space arg)
                                                      (format nil "~s" arg)
                                                      arg))
                                                (ensure-list command)))))
    (when *verbose*
      (format-command t command))
    (format-command log-stream command)

    (let* ((out    (make-broadcast-stream log-stream))
           (errors (make-string-output-stream))
           (err    (make-broadcast-stream log-stream errors)))
      (uiop:with-current-directory (cwd)
        (multiple-value-bind (output error code)
            (uiop:run-program command
                              :output out
                              :error-output err
                              :ignore-error-status t)
          (declare (ignore output error))
          (unless ignore-error-status
            (unless (= 0 code)
              (format t "~%Command:  ~a" (format-command nil command))
              (format t "Status: ~a~%" code)
              (format t "Error: ~a: ~a~%"
                      (car (ensure-list command))
                      (get-output-stream-string errors))
              (error "Command ~s failed with status ~a."
                     (car (ensure-list command))
                     code)))

          ;; return the error code, as we don't have output/error anymore
          code)))))
