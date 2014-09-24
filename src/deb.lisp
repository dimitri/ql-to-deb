;;;
;;; Update our debian packages with the latest known Quicklisp releases.
;;;
(in-package #:ql-to-deb)

(defstruct (debian-package
             (:conc-name deb-))
  (system   nil :type string)            ; the quicklisp system it relates too
  (dir      nil :type pathname)          ; our own debian/ directory
  (source   nil :type (or null string))  ; the source package name
  (version  nil :type (or null string))  ; debian/changelog first version string
  (revision nil :type (or null string))  ; debian/changelog revision string
  (package  nil :type (or null string))  ; debian package name
  (sid-version  nil :type (or null string)) ; sid version
  )

(defmethod read-version ((deb debian-package))
  "Parse the debian/changelog for the current version of the package."
  (let ((changelog (make-pathname :defaults (deb-dir deb)
                                  :name "changelog")))
    (when (probe-file changelog)
      (let ((first-line (with-open-file (s changelog)
                          (read-line s))))
        (cl-ppcre:register-groups-bind (version revision)
            ("[^ ]+ \\(([^-]+)-([^\\)]+).*" first-line)
          (values version revision))))))

(defmethod full-version ((deb debian-package))
  "Return the full text of the local-version of the DEB pacakge."
  (format nil "~a-~a" (deb-version deb) (deb-revision deb)))

(defmethod read-source-name ((deb debian-package))
  "Parse debian/control first line for the name of the source package."
  (let ((control (make-pathname :defaults (deb-dir deb) :name "control")))
    (let ((first-line (with-open-file (s control) (read-line s))))
      (cl-ppcre:register-groups-bind (source)
          ("Source: (.*)" first-line)
        source))))

(defmethod complete-debian-package ((deb debian-package))
  "Compute missing elements in the package."

  ;; it's only possible to do that when we have the bare minimum information
  ;; already, that is the name of the package and its debian/ directory
  ;; pathname
  (when (and (slot-boundp deb 'system)
             (slot-boundp deb 'dir))
    ;; first, the main system name is either the package name or the
    ;; package name prefixed with cl-
    (with-slots (system) deb
      (let ((package (if (and (< 3 (length system))
                              (string= "cl-" system :end2 3))
                         system
                         (format nil "cl-~a" system))))
        (setf (deb-package deb) package)))

    ;; grab the debian package's source name from debian/control
    (setf (deb-source deb) (read-source-name deb))

    ;; and now go fetch the current version in the debian/changelog file if
    ;; such does exists.
    (multiple-value-bind (version revision)
        (read-version deb)
      (setf (deb-version deb) version
            (deb-revision deb) revision))))

(defun find-debian-package (package-name)
  "Find PACKAGE-NAME in *DEBIAN-PACKAGES* directory."
  (let* ((pdir (make-pathname :directory `(:relative ,package-name "debian")))
         (pdir (merge-pathnames pdir *debian-packages*)))
    (when (probe-file pdir)
      (let ((package (make-debian-package :system package-name :dir pdir)))
        (complete-debian-package package)
        package))))

(defun list-debian-packages ()
  "Walk the *DEBIAN-PACKAGES* directory and return a list of debian-package."
  (loop :for dir :in (uiop:directory-files *debian-packages*)
     :collect (let* ((system  (last-directory dir))
                     (debian  (uiop:merge-pathnames* "debian/" dir))
                     (package (make-debian-package :system system
                                                   :dir debian)))
                (complete-debian-package package)
                package)))

(defmethod build-directory ((deb debian-package))
  (let ((source (make-pathname :directory `(:relative ,(deb-source deb)))))
    (merge-pathnames source *build-root*)))

(defmethod build-changelog ((deb debian-package))
  (uiop:merge-pathnames* "debian/changelog" (build-directory deb)))

(defmethod source-changelog ((deb debian-package))
  (merge-pathnames "changelog" (deb-dir deb)))


;;;
;;; Dealing with epoch in debian version strings
;;;
(defmethod version-and-epoch ((deb debian-package))
  "Return epoch and version string as two different values."
  (cl-ppcre:register-groups-bind (epoch version)
      ("([0-9]*:)?(.*)" (deb-version deb))
    (values version epoch)))

(defmethod next-epoch ((deb debian-package))
  (multiple-value-bind (version epoch)
      (version-and-epoch deb)
    (declare (ignore version))
    (+ 1 (parse-integer (or epoch "0")))))

(defmethod package-changes-namestring ((deb debian-package) arch)
  "Return the pathname of the debian .changes file for DEB on ARCH."
  (let ((filename (format nil "~a_~a_~a.changes"
                          (deb-source deb) (full-version deb) arch)))
    (uiop:native-namestring (uiop:merge-pathnames* filename *build-root*))))


;;;
;;; Using debian utilities
;;;
(defun rmadison (package-list &key (suite "sid"))
  "Run the `rmadison` command on given PACKAGE-LIST."
  (let* ((source-list  (mapcar #'deb-source package-list))
         (rmadison    `("rmadison" "-s" ,suite ,@source-list)))
    (multiple-value-bind (output error code)
        (uiop:run-program rmadison :output :string :error-output :string)
      ;; now parse the output
      (declare (ignore error code))
      (alexandria:alist-hash-table
       (with-input-from-string (s output)
         (loop :for line := (read-line s nil nil)
            :while line
            :collect (destructuring-bind (name version suite &rest details)
                         (remove "" (cl-ppcre:split "[\\| ]"  line) :test #'string=)
                       (declare (ignore suite details))
                       (cons name version))))
       :test 'equal))))

(defmethod debuild ((deb debian-package))
  "Use the command `debuild -us -uc' to build given DEB package."
  (let ((debuild `("debuild" "-us" "-uc")))
    (format t "Building package ~a~%" (deb-source deb))
    (run-command debuild (build-directory deb))))

(defmethod compute-next-version ((deb debian-package) new-version)
  "We might need to increment the epoch, it is taken care of here."
  (if (deb-version deb)
      (multiple-value-bind (version epoch)
          (version-and-epoch deb)
       (let ((compare `("dpkg"
                        "--compare-versions"
                        ,new-version
                        "gt"
                        ,version)))
         (if (= 0 (run-command compare (deb-dir deb) :ignore-error-status t))
             (setf (deb-version deb)
                   (if epoch (format nil "~a~a" epoch new-version)
                       new-version))

             ;; bump epoch!
             (progn
               (setf (deb-version deb)
                     (format nil "~d:~a" (next-epoch deb) new-version))
               (format t "Epoch bump needed, new version is ~s~%" (deb-version deb))))))

      ;; no pre-existing debian version, just take the Quicklisp version string
      (setf (deb-version deb) new-version))
  deb)

