;;;; pginstall.asd

(asdf:defsystem #:ql-to-deb
    :serial t
    :description "Automated debian package updates from Quicklisp releases"
    :author "Dimitri Fontaine <dim@tapouen.org>"
    :license "WTFPL"
    :version "0.4.0"
    :depends-on (#:uiop			; host system integration
		 #:drakma		; http client, download archives
                 #:cl-ppcre             ; Regular Expressions
                 #:split-sequence       ; split sequences
                 #:md5                  ; check archive checksums
		 #:command-line-arguments ; for the main function
                 )
    :components
    ((:module "src"
              :serial t
	      :components
              ((:file "package")
               (:file "params")
               (:file "utils")
               (:file "ql")
               (:file "deb")
               (:file "ql-to-deb")
               (:file "main")))))

