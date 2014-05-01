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
  (let* ((pdir (make-pathname :directory `(:relative ,package-name)))
         (pdir (merge-pathnames pdir *debian-packages*)))
    (when (probe-file pdir)
      (let ((package (make-debian-package :system package-name :dir pdir)))
        (complete-debian-package package)
        package))))

(defun list-debian-packages ()
  "Walk the *DEBIAN-PACKAGES* directory and return a list of debian-package."
  (let ((root     *debian-packages*)
        (packages '()))
    (flet ((add-package (name-key kind parent depth)
             (declare (ignore parent))
             (when (and (= depth 1) (eq kind :directory))
               (let* ((system  (file-path-namestring name-key))
                      (curdir  (make-pathname :directory `(:relative ,system)))
                      (curdir  (merge-pathnames curdir root))
                      (debian  (make-pathname :directory '(:relative "debian")))
                      (debian-pathname (merge-pathnames debian curdir))
                      (package
                       (make-debian-package :system system
                                            :dir debian-pathname)))
                 (complete-debian-package package)
                 (push package packages)))))
      (walk-directory root #'add-package))
    (nreverse packages)))

