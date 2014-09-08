;;;
;;; sbcl --load /vagrant/conf/start-swank-server.lisp
;;;

(load (merge-pathnames "quicklisp/setup.lisp"
                       (user-homedir-pathname)))
(ql:quickload :swank)
(let ((swank::*loopback-interface* "0.0.0.0"))
  (swank:create-server :port 4205 :style :spawn :dont-close t))
