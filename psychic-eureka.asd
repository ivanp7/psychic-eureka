;;;; psychic-eureka.asd

(asdf:defsystem #:psychic-eureka
  :description "Abstract text animations"
  :author "Ivan Podmazov <ivanpzv8@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :depends-on (#:alexandria)
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:static-file "roswell/psychic-eureka.ros")
               (:file "package") 
               (:file "ll" :depends-on ("package"))
               (:file "test" :depends-on ("ll"))))

