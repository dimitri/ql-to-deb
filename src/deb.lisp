;;;
;;; Update our debian packages with the latest known Quicklisp releases.
;;;
(in-package #:ql-to-deb)

(defstruct (debian-package
             (:conc-name deb-))
  (system  nil :type string)
  (dir     nil :type pathname)
  (version nil :type (or null string))
  (package nil :type (or null string)))

(defmethod read-version ((deb debian-package))
  "Parse the debian/changelog for the current version of the package."
  (let ((changelog (make-pathname :defaults (deb-dir deb)
                                  :name "changelog")))
    (when (probe-file changelog)
      (let ((first-line (with-open-file (s changelog)
                          (read-line s))))
        (cl-ppcre:register-groups-bind (version)
            ("[^ ]+ \\(([^-]+)-[^\\)]+.*" first-line)
          version)))))

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

    ;; and now go fetch the current version in the debian/changelog file if
    ;; such does exists.
    (setf (deb-version deb) (read-version deb))))

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


;;;
;;; Using debian utilities
;;;
(defmethod debuild ((deb debian-package))
  "Use the command `debuild -us -uc' to build given DEB package."
  (let* ((pdir    (make-pathname :directory `(:relative ,(deb-package deb))))
         (pdir    (merge-pathnames pdir *build-root*))
         (debuild `("debuild" "-us" "-uc")))
    (format t "Building package ~a~%" (deb-package deb))
    (run-command debuild pdir)))

(defmethod next-epoch ((deb debian-package))
  (cl-ppcre:register-groups-bind (epoch version)
      ("([0-9]*:)?(.*)" (deb-version deb))
    (declare (ignore version))
    (+ 1 (parse-integer (or epoch "0")))))

(defmethod compute-next-version ((deb debian-package) new-version)
  "We might need to increment the epoch, it is taken care of here."
  (let ((compare `("dpkg"
                   "--compare-versions"
                   ,new-version
                   "gt"
                   ,(deb-version deb))))
    (if (= 0 (run-command compare (deb-dir deb) :ignore-error-status t))
        (setf (deb-version deb) new-version)

        ;; bump epoch!
        (progn
         (setf (deb-version deb)
               (format nil "~d:~a" (next-epoch deb) new-version))
         (format t "Epoch bump needed, new version is ~s~%" (deb-version deb)))))
  deb)

