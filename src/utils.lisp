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
  (uiop:with-current-directory ((uiop:pathname-directory-pathname source))
    (multiple-value-bind (output error code)
        (uiop:run-program `("ln" "-s"
                                 ,(uiop:native-namestring source)
                                 ,(uiop:native-namestring target))
                          :output :string
                          :error-output :string)
      (declare (ignore output error code)))))

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
                                                command))))
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
                      (car command)
                      (get-output-stream-string errors))
              (error "Command ~s failed with status ~a." (car command) code)))

          ;; return the error code, as we don't have output/error anymore
          code)))))
