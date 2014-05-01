;;;
;;; Update our debian packages with the latest known Quicklisp releases.
;;;
(in-package #:ql-to-deb)

(defmethod same-version-p ((deb debian-package) (ql ql-release))
  "Return a generalized boolean that is non-nil when both the debian package
  and the ql release have the same current version number."
  (when (deb-version deb)
    (string= (deb-version deb) (ql-version ql))))

(defun maybe-update-package (package-name)
  "Check if given PACKAGE-NAME needs an update, then do it if needed."
  (let ((package  (find-debian-package package-name))
        (releases (ql-fetch-current-releases)))
    (if package
        (let ((release (gethash (deb-system package) releases)))
         (if release
             (if (same-version-p package release)
                 (format t "package ~a is already up to date (~a)~%"
                         (deb-package package)
                         (ql-version release))
                 (update-package package release))

             ;; not found in the releases hash
             (format t "Missing a Quicklisp release for package: ~s~%"
                     package-name)))

        ;; no package found
        (format t "Could not find a debian package named: ~a~%" package-name))))

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

  (format t "updating package ~a to version ~a~%"
          (deb-package deb) (ql-version ql))

  ;; fetch the archive, unpack it
  (ql-fetch-and-unpack-release ql)

  ;; rename the archive and add the debian directory in its directory
  (package-release deb ql)

  ;; add a debian changelog entry
  (update-changelog deb ql)

  ;; now debuild the package
  (debuild deb))


;;;
;;; Lower level bits of Quicklisp to debian
;;;
(defmethod package-release ((deb debian-package) (release ql-release))
  "Transform a Quicklisp release to a ready to build debian package."
  ;; we need the infamous debian orig tarball
  (let* ((orig-filename
          (format nil "~a_~a.orig.tar.gz" (deb-package deb) (ql-version release)))
         (debian-archive-filename
          (merge-pathnames orig-filename *build-root*)))
    ;; get rid of possibly existing stray symlinks from previous runs
    (when (probe-file debian-archive-filename)
      (delete-file debian-archive-filename))
    (make-symlink (ql-archive release) debian-archive-filename))

  ;; rename the directory in which the archive has been expanded
  (let* ((ql-dir  (merge-pathnames (ql-prefix release) *build-root*))
         (deb-dir (make-pathname :directory `(:relative ,(deb-package deb))))
         (deb-dir (merge-pathnames deb-dir *build-root*)))
    ;; remove possibly existing stray target directory
    (uiop:delete-directory-tree deb-dir :validate t :if-does-not-exist :ignore)
    (rename-file ql-dir (deb-package deb)))

  ;; now just copy the debian/ directory and all its contents in place into
  ;; the unpacked directory where we find the release
  (let* ((dir (make-pathname :directory `(:relative ,(deb-package deb))))
         (dir (merge-pathnames dir *build-root*)))
    (multiple-value-bind (output error code)
        (uiop:run-program `("cp" "-a"
                                 ,(directory-namestring (deb-dir deb))
                                 ,(directory-namestring dir)))
      (declare (ignore output error code))))

  ;; side effects only, no return value
  (values))

(defmethod update-changelog ((deb debian-package) (ql ql-release))
  "Update the debian/changelog for DEB package, at *BUILD-ROOT*."
  (let* ((pdir        (make-pathname :directory `(:relative ,(deb-package deb))))
         (pdir        (merge-pathnames pdir *build-root*))
         (changelog   (merge-pathnames "debian/changelog" pdir)))
    (uiop:with-current-directory (pdir)
      (multiple-value-bind (output error code)
          (uiop:run-program `("dch"
                              ,@(unless (probe-file changelog) (list "--create"))
                              "--newversion" ,(format nil "~a-1" (ql-version ql))
                              "--package"    ,(deb-package deb)
                              "Quicklisp release update.")
                            :output :string
                            :error-output :string)
        (declare (ignore output error code))))))
