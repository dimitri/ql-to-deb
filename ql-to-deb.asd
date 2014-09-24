;;;; pginstall.asd

(asdf:defsystem #:ql-to-deb
    :serial t
    :description "Automated debian package updates from Quicklisp releases"
    :author "Dimitri Fontaine <dim@tapoueh.org>"
    :license "WTFPL"
    :version "0.6.0"
    :depends-on (#:uiop			; host system integration
		 #:drakma		; http client, download archives
                 #:cl-ppcre             ; Regular Expressions
                 #:split-sequence       ; split sequences
                 #:md5                  ; check archive checksums
		 #:command-line-arguments ; for the main function
                 #:py-configparser      ; read .ini config files
                 #:alexandria           ; some utils
                 )
    :components
    ((:module "src"
	      :components
              ((:file "package")
               (:file "params" :depends-on ("package"))
               (:file "utils"  :depends-on ("package"))
               (:file "ql"     :depends-on ("package" "params" "utils"))
               (:file "deb"    :depends-on ("package" "params" "utils"))
               (:file "ql-to-deb" :depends-on ("package"
                                               "params"
                                               "utils"
                                               "ql"
                                               "deb"))
               (:file "commands" :depends-on ("package"
                                              "params"
                                              "utils"
                                              "ql"
                                              "deb"
                                              "ql-to-deb"))
               (:file "main" :depends-on ("package"
                                          "params"
                                          "utils"
                                          "ql"
                                          "deb"
                                          "ql-to-deb"
                                          "commands"))))))

