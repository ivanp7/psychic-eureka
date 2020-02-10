;;;; package.lisp

(defpackage #:psychic-eureka
  (:use #:cl)
  (:export :color :coord :rgb-buffer :*buffer* :*buffer-update-fn*
           :initialize-screen :uninitialize-screen :display-buffer))

