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

(defun run-command (command cwd &optional (log-stream *log-stream*))
  "Run specified COMMAND (a list of strings) within CWD."
  (flet ((format-command (stream command)
           (format stream "~{~a~^ ~}~%" (mapcar (lambda (arg)
                                                  (if (find #\Space arg)
                                                      (format nil "~s" arg)
                                                      arg))
                                                command))))
   (when *verbose*
     (format-command t command))
   (format-command log-stream command))

  (let ((cs (make-broadcast-stream log-stream)))
    (uiop:with-current-directory (cwd)
      (multiple-value-bind (output error code)
          (uiop:run-program command :output cs :error-output cs)
        (declare (ignore output error code))))))
