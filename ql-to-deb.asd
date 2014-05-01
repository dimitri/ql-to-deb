;;;; pginstall.asd

(asdf:defsystem #:ql-to-deb
    :serial t
    :description "Automated debian package updates from Quicklisp releases"
    :author "Dimitri Fontaine <dim@tapouen.org>"
    :license "WTFPL"
    :depends-on (#:uiop			; host system integration
		 #:drakma		; http client, download archives
                 #:cl-ppcre             ; Regular Expressions
                 #:split-sequence       ; split sequences
                 #:iolib                ; OS integration
                 #:iolib/os             ; OS integration
                 #:iolib/pathnames      ; walk directory and all
                 #:md5                  ; check archive checksums
		 #:command-line-arguments ; for the main function
                 )
    :components
    ((:module "src"
              :serial t
	      :components
              ((:file "package")
               (:file "params")
               (:file "ql")
               (:file "deb")
               (:file "ql-to-deb")
               (:file "main")))))

