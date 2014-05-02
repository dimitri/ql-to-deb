;;;
;;; Update our debian packages with the latest known Quicklisp releases.
;;;
(in-package #:ql-to-deb)

(defmethod same-version-p ((deb debian-package) (ql ql-release))
  "Return a generalized boolean that is non-nil when both the debian package
  and the ql release have the same current version number."
  (when (deb-version deb)
    (multiple-value-bind (epoch version)
        (epoch-and-version deb)
      (declare (ignore epoch))
      (string= version (ql-version ql)))))

(defun ensure-debian-package-list (packages)
  "Returns a lisp of debian-package instances."
  (loop :for package-or-name :in packages
     :for package := (etypecase package-or-name
                       (string         (find-debian-package package-or-name))
                       (debian-package package-or-name))
     :when package
     :collect package
     :else
     :do (format t "Could not find a debian package named: ~a.~%"
                 package-or-name)))

(defun filter-packages-to-update (packages)
  "Return the list of packages that needs an update."
  (let ((releases (ql-fetch-current-releases)))
    (loop :for package :in (ensure-debian-package-list packages)
       :for release := (gethash (deb-system package) releases)

       :when (null release)
       :do (format t "Missing a Quicklisp release for package: ~s.~%" package)

       :when (same-version-p package release)
       :do (format t "~a is already up to date (~a).~%"
                   (deb-source package)
                   (ql-version release))

       :unless (or (null release) (same-version-p package release))
       :collect (cons package release))))

(defun list-packages-to-update ()
  "Walk the *DEBIAN-PACKAGES* directory, fetch the lastest Quicklisp release
   distributions, and return a list of packages that need a new version."
  (filter-packages-to-update (list-debian-packages)))

(defun update-package (deb ql)
  "Given a debian package DEB and a Quicklisp release QL, update the debian
   package with the contents of the archive."
  (declare (type debian-package deb)
           (type ql-release ql))

  (format t "~%Updating package ~a~@[ from ~a~] to ~a.~%"
          (deb-source deb) (deb-version deb) (ql-version ql))

  (let ((log-pathname (make-pathname :directory *logs-root*
                                     :name (deb-source deb)
                                     :type "log")))
    (ensure-directories-exist *logs-root*)

    (format t "     see logs in ~s~%" (namestring log-pathname))

    (with-open-file (*log-stream* log-pathname
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)

      (handler-case
          (progn
            ;; fetch the archive, unpack it
            (ql-fetch-and-unpack-release ql)

            ;; rename the archive and add the debian directory in its directory
            (package-release deb ql)

            ;; add a debian changelog entry
            (update-changelog deb ql)

            ;; now debuild the package
            (debuild deb)

            ;; copy the new changelog file to our debian packaging source
            (let ((cp `("cp" ,(namestring (build-changelog deb))
                             ,(namestring (source-changelog deb)))))
              (run-command cp (deb-dir deb))))

        ;; just ensure we keep the log file when something happens
        (condition (c)
          (format *log-stream* "Fatal: ~a~%" c)
          (format t "Fatal: ~a~%" c))))))


;;;
;;; Lower level bits of Quicklisp to debian
;;;
(defmethod package-release ((deb debian-package) (release ql-release))
  "Transform a Quicklisp release to a ready to build debian package."
  ;; we need the infamous debian orig tarball
  (let* ((orig-filename
          (format nil "~a_~a.orig.tar.gz" (deb-source deb) (ql-version release)))
         (debian-archive-filename
          (merge-pathnames orig-filename *build-root*)))
    ;; get rid of possibly existing stray symlinks from previous runs
    (when (probe-file debian-archive-filename)
      (delete-file debian-archive-filename))
    (make-symlink (ql-archive release) debian-archive-filename))

  ;; rename the directory in which the archive has been expanded
  (let* ((ql-dir  (merge-pathnames (ql-prefix release) *build-root*))
         (deb-dir (make-pathname :directory `(:relative ,(deb-source deb))))
         (deb-dir (merge-pathnames deb-dir *build-root*))
         (rename  `("mv" ,(namestring ql-dir) ,(deb-source deb))))
    ;; remove possibly existing stray target directory
    (uiop:delete-directory-tree deb-dir :validate t :if-does-not-exist :ignore)
    (run-command rename *build-root*))

  ;; now just copy the debian/ directory and all its contents in place into
  ;; the unpacked directory where we find the release
  (let* ((dir (uiop:merge-pathnames* (deb-source deb) *build-root*))
         (cp  `("cp" "-a"
                     ,(string-right-trim "/" (namestring (deb-dir deb)))
                     ,(namestring dir))))
    (run-command cp *build-root*))

  ;; side effects only, no return value
  (values))

(defmethod update-changelog ((deb debian-package) (ql ql-release))
  "Update the debian/changelog for DEB package, at *BUILD-ROOT*."
  (let* ((pdir        (build-directory deb))
         (changelog   (build-changelog deb)))

    ;; we might need to bump epoch here
    (compute-next-version deb (ql-version ql))

    ;; now that we have a proper epoch, go on to calling dch.
    (let ((dch         `("dch"
                         ,@(unless (probe-file changelog) (list "--create"))
                         "--newversion" ,(format nil "~a-1" (deb-version deb))
                         "--package"    ,(deb-source deb)
                         "--distribution" "unstable"
                         "--controlmaint"
                         "Quicklisp release update.")))

      ;; run dch then update debian's package version string
      (run-command dch pdir))))
