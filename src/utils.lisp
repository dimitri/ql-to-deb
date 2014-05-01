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
