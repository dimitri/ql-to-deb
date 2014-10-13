;;;
;;; Main commands available from the command line.
;;;

(in-package #:ql-to-deb)

(defun filter-package-list (package-list packages)
  "Filter PACKAGE-LIST to only keep given PACKAGES names."
  (if packages
      (remove-if-not (lambda (deb) (member (deb-system deb) packages
                                           :test #'string=))
                     package-list)
      package-list))

(defun fetch-current-status (debian-package-list)
  "Given a list of debian packages, retrieve their status."
  (let ((ql-status     (ql-fetch-current-releases))
        (debian-status (rmadison debian-package-list)))
    (loop :for package :in debian-package-list
       :for source      := (deb-source package)
       :for system      := (deb-system package)
       :for sid-version := (gethash source debian-status)
       :for release     := (gethash system ql-status)
       :do (setf (deb-sid-version package) sid-version)
       :collect (list package release))))

(defun package-needs-update (package release)
  "Return a generalized boolean, non-nil when PACKAGE version is different
   than the Quicklisp version of RELEASE."
  (not (same-version-p package release)))

(defun package-needs-upload (package)
  "Return a generalized boolean, non-nil when PACKAGE local on-disk version
   is different from the version in sid."
  (not (string= (deb-sid-version package) (full-version package))))

(defun clean-changes-pathname ()
  "Remove the *changes-pathname* file, cleaning the cache."
  (when (probe-file *changes-pathname*)
    (delete-file *changes-pathname*)))

(defun status (&key
                 packages
                 (package-list (list-debian-packages)))
  "Fetch and display current package list status."
  (let ((current-status
         (fetch-current-status (filter-package-list package-list packages))))
   (format t  "~a~35t~a~55t~a~73t~a~%"
           "  Package"   " sid version"  " local version"  " ql version")
   (format t  "~a~35t~a~55t~a~73t~a~%"
           "-----------" "-------------" "---------------" "------------")

   (loop :for (package release) :in current-status
      :for source  := (deb-source package)
      :for system  := (deb-system package)
      :for sid-version := (deb-sid-version package)
      :do (format t "~:[✗~;✓~] ~a~36t~a~56t~a~74t~a~%"
                  (and (not (package-needs-update package release))
                       (not (package-needs-upload package)))
                  source
                  (or sid-version "")
                  (full-version package)
                  (ql-version release)))))

(defun check (&key
                packages
                (package-list (list-debian-packages)))
  "Display the list of packages in need for some care and love."
  (let ((current-status
         (fetch-current-status (filter-package-list package-list packages))))
    (clean-changes-pathname)
    (loop :for (package release) :in current-status
       :when (package-needs-update package release)
       :collect (deb-system package) :into needs-update

       :when (package-needs-upload package)
       :collect (deb-system package) :into needs-upload

       :finally (progn
                  (when needs-update
                    (format t "update: ~{~a~^ ~}~%" needs-update))
                  (when needs-upload
                    (format t "upload: ~{~a~^ ~}~%" needs-upload))))))

(defun update (&key
                 packages
                 (package-list (list-debian-packages)))
  "Rebuild all packages that need to be updated."
  (let ((current-status
         (fetch-current-status (filter-package-list package-list packages)))
        (arch (dpkg-architecture)))
    (with-open-file (changes *changes-pathname*
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create
                             :external-format :utf-8)
      (loop :for (package release) :in current-status
         :when (or (and packages *fix-bugs*)
                   (package-needs-update package release))
         :do (progn
               (update-package package release)
               (format changes "~a~%" (package-changes-namestring package arch)))))))

(defun just-built-package-changes-list ()
  "Return the list of the packages .changes filenames that we just built."
  (when (probe-file *changes-pathname*)
   (with-open-file (changes *changes-pathname*
                            :direction :input
                            :external-format :utf-8)
     (loop :for line := (read-line changes nil nil)
        :while line
        :collect line))))

(defun list-packages-change-files (package-list)
  "Return a list of debian .changes filenames for PACKAGE-LIST, a list of
   source package names ."
  (let ((arch (dpkg-architecture)))
    (loop :for deb :in (list-debian-packages)
       :when (member (deb-system deb) package-list :test #'string=)
       :collect (package-changes-namestring deb arch))))

(defun lint (&key packages)
  "Call `lintian` on each package we just built."
  (let* ((changes-list (if packages
                           (list-packages-change-files packages)
                           (just-built-package-changes-list))))
    (loop :for change-filename :in changes-list
       :do (progn
             ;; lint should always be verbose actually
             (format t "lintian ~s~%" change-filename)
             (lintian change-filename)))))

(defun sign (&key packages)
  "Call `debsign` on each package we just built."
  (let* ((changes-list (if packages
                           (list-packages-change-files packages)
                           (just-built-package-changes-list))))
    (loop :for change-filename :in changes-list
       :do (progn
             (when *verbose*
               (format t "debsign ~s~%" change-filename))
             (debsign change-filename)))))

(defun upload (&key packages)
  "Call `dput` on each package we just built."
  (let* ((changes-list (if packages
                           (list-packages-change-files packages)
                           (just-built-package-changes-list))))
    (loop :for change-filename :in changes-list
       :do (progn
             (when *verbose*
               (format t "dput ~s~%" change-filename))
             (dput change-filename)))))

(defun install (&key packages)
  "Call `sudo dpkg -i` on each package we just built."
  (let* ((changes-list (if packages
                           (list-packages-change-files packages)
                           (just-built-package-changes-list)))
         (deb-files
          (loop :for change-filename :in changes-list
             :append (parse-changes-files change-filename "\\.deb$"))))
    (format t "sudo dpkg -i ~{~a~^ ~}~%" deb-files)
    (dpkg-i deb-files)))
