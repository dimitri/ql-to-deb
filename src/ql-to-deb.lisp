;;;
;;; Update our debian packages with the latest known Quicklisp releases.
;;;
(in-package #:ql-to-deb)

(defmethod same-version-p ((deb debian-package) (ql ql-release))
  "Return a generalized boolean that is non-nil when both the debian package
  and the ql release have the same current version number."
  (when (deb-version deb)
    (string= (deb-version deb) (ql-version ql))))

(defun list-packages-to-update ()
  "Walk the *DEBIAN-PACKAGES* directory, fetch the lastest Quicklisp release
   distributions, and return a list of packages that need a new version."
  (let ((packages (list-debian-packages))
        (releases (ql-fetch-current-releases)))
    (loop :for package :in packages
       :for release := (gethash (deb-system package) releases)
       :when (null release)
       :do (format t "Missing a Quicklisp release for package: ~s~%" package)
       :unless (or (null release)
                   (same-version-p package release))
       :collect (cons package release))))

(defun update-package (deb ql)
  "Given a debian package DEB and a Quicklisp release QL, update the debian
   package with the contents of the archive."
  (declare (type debian-package deb)
           (type ql-release ql))

  (let* ((ql-prefix-path (make-pathname :directory `(:relative ,(ql-prefix ql))))
         (package-root   (merge-pathnames ql-prefix-path *build-root*)))
    (ensure-directories-exist package-root)
    ;; fetch the archive, rename it, unpack it
    ;; add the debian directory
    ;; add a debian changelog entry
    ))
