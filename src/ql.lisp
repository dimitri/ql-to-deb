;;;
;;; Update our debian packages with the latest known Quicklisp releases.
;;;
(in-package #:ql-to-deb)

;;;
;;; First, some parsing tools for the expected data from quicklisp
;;;

(defparameter *sample-property-string*
  "name: quicklisp
version: 2014-04-25
system-index-url: http://beta.quicklisp.org/dist/quicklisp/2014-04-25/systems.txt
release-index-url: http://beta.quicklisp.org/dist/quicklisp/2014-04-25/releases.txt
archive-base-url: http://beta.quicklisp.org/
canonical-distinfo-url: http://beta.quicklisp.org/dist/quicklisp/2014-04-25/distinfo.txt
distinfo-subscription-url: http://beta.quicklisp.org/dist/quicklisp.txt")

(defparameter *sample-release-string*
  "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]
3b-swf http://beta.quicklisp.org/archive/3b-swf/2012-01-07/3b-swf-20120107-git.tgz 75453 c130b35b44de03caa7cdf043d9d27dee 2dc75635c4d6a641618a63e61a9aa98252bbd2b7 3b-swf-20120107-git 3b-swf-swc.asd 3b-swf.asd
3bil http://beta.quicklisp.org/archive/3bil/2012-01-07/3bil-20120107-git.tgz 271517 db71eeab473eb016a56ff0a13cfc9174 818033d299d6b062d12806ac2ec70a0fd852a0a4 3bil-20120107-git 3b-swf-writer.asd avm2-asm.asd avm2-compile.asd avm2-lib.asd swf-writer-hack.asd
3bmd http://beta.quicklisp.org/archive/3bmd/2014-01-13/3bmd-20140113-git.tgz 15403 3ba0c96b76555145cc6f155c98005810 7b5227deac0c11bb867bed3b3ecf8ade4e546b59 3bmd-20140113-git 3bmd-ext-code-blocks.asd 3bmd-ext-definition-lists.asd 3bmd-ext-wiki-links.asd 3bmd.asd
drakma http://beta.quicklisp.org/archive/drakma/2014-04-25/drakma-1.3.8.tgz 70983 4e80f021b6e21c8db9d5dd6246602d53 d18893ba81569d40e7a25591094aeb976ba67437 drakma-1.3.8 drakma-test.asd drakma.asd")

(defun ql-parse-properties (properties-string)
  "Parse the PROPERTIES string into a proper alist"
  (with-input-from-string (s properties-string)
    (loop :for line := (read-line s nil nil)
       :while line
       :collect (cl-ppcre:register-groups-bind (name property)
                    ("([a-z-]+): (.*)" line)
                  (cons name property)))))

(defun ql-property (property properties)
  "Returns the PROPERTY value within PROPERTIES."
  (cdr (assoc property properties :test #'string=)))

(defun ql-parse-version-string (project prefix)
  "Return the upstream version string given the release prefix"
  (cl-ppcre:register-groups-bind (version)
      ((format nil "~a[-_.]([0-9.]+)(?:-.*)?" project) prefix)
    version))

(defstruct (ql-release
             (:conc-name ql-)
             (:constructor make-ql-release
                           (project url size file-md5
                                    content-sha1 prefix
                                    &rest systems
                                    &aux (version
                                          (ql-parse-version-string project prefix)))))
  ;; found in the releases.txt file
  project version url size file-md5 content-sha1 prefix systems
  ;; added while working on the packaging the release
  archive)

(defun ql-parse-releases (releases-string)
  "Parse the RELEASES-STRING content"
  (with-input-from-string (s releases-string)
    (loop :for line := (read-line s nil nil)
       :while line
       :when (and line
                  (< 0 (length line))
                  (not (char= (aref line 0) #\#)))
       :collect (apply #'make-ql-release (split-sequence #\Space line)))))


;;;
;;; Talk to the Quicklisp archives (static files, actually)
;;;

(define-condition quicklisp-http-error ()
  ((uri         :initarg :uri    :reader server-error-uri)
   (status-code :initarg :status :reader server-error-status-code)
   (reason      :initarg :reason :reader server-error-reason)
   (body        :initarg :body   :reader server-error-body)))

(defun ql-fetch-current-properties ()
  "Fetch the current Quicklisp releases."
  (format t "Fetching ~s~%" *ql-props-url*)
  (multiple-value-bind (body status-code headers uri stream must-close reason)
      (drakma:http-request *ql-props-url*)
    (declare (ignore headers stream must-close))
    (if (= status-code 200)
        (ql-parse-properties body)
        (error 'quicklisp-http-error
               :uri uri :status status-code :reason reason :body body))))

(defun ql-fetch-current-releases ()
  "Fetch the current Quicklisp releases."
  (let* ((properties  (ql-fetch-current-properties))
         (release-url (ql-property *ql-release-property* properties)))
    (format t "Fetching ~s~%" release-url)
   (multiple-value-bind (body status-code headers uri stream must-close reason)
       (drakma:http-request release-url)
     (declare (ignore headers stream must-close))
     (if (= status-code 200)
         (let ((ql-releases-hash (make-hash-table :test 'equal)))
           (loop :for release :in (ql-parse-releases body)
              :do (setf (gethash (ql-project release) ql-releases-hash) release))
           ql-releases-hash)
         (error 'quicklisp-http-error
                :uri uri :status status-code :reason reason :body body)))))


;;;
;;; Archive fetching and unpacking
;;;
(defparameter *archive-buffer-size* 8192)

(defmethod validate-checksum ((release ql-release))
  "Return a generalized boolean that is non-nil when RELEASE computed md5sum
   is the same as its 'file-md5 slot value."
  (when (ql-archive release)
    (let* ((md5        (md5:md5sum-file (ql-archive release)))
           (md5-string (format nil "~(~{~2,'0X~}~)" (coerce md5 'list))))
      (if (string= (ql-file-md5 release) md5-string)
          (format t "Checksum test passed.~%~5TFile: ~s~%~5T md5: ~a~%"
                  (uiop:native-namestring (ql-archive release))
                  md5-string)
          (error 'quicklisp-http-error
                 :uri (ql-url release)
                 :reason (format nil "checksum failure, expected ~s, got ~s~%"
                                 (ql-file-md5 release) md5-string))))))

(defun ql-fetch-release (release)
  "Download given RELEASE archive file in *ARCHIVE-DIRECTORY*."
  (ensure-directories-exist (directory-namestring *archive-directory*))

  (let* ((filename (make-pathname :name (pathname-name (ql-url release))
                                  :type "tgz"))
         (archive  (merge-pathnames filename *archive-directory*)))

    ;; remember where we did download the archive to
    (setf (ql-archive release) archive)

    ;; first download the archive, a .tgz
    (format t "Fetching ~s~%" (ql-url release))
    (multiple-value-bind (body status-code headers uri stream must-close reason)
        (drakma:http-request (ql-url release))

      (declare (ignore headers stream must-close))
      (unless (= 200 status-code)
        (error 'quicklisp-http-error :uri uri :status status-code :reason reason))

      (with-open-file (out archive
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :element-type '(unsigned-byte 8))
        (write-sequence body out)))

    ;; check that the archive has the expected checksum
    (validate-checksum release)

    ;; return the archive pathname we just created
    archive))

(defun ql-unpack-archive (release)
  "Unpack already fetched RELEASE in *BUILD-ROOT*"
  (ensure-directories-exist (directory-namestring *build-root*))

  (let* ((archive     (ql-archive release))
         (tar        `("tar" "xzf" ,(namestring archive))))
    (run-command tar *build-root*)))

(defmethod ql-fetch-and-unpack-release ((release ql-release))
  "Fetches release from its URL slot value into *ARCHIVE-DIRECTORY* then
   unpack it in the same place."
  (ql-fetch-release release)
  (ql-unpack-archive release))
