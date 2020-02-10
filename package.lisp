;;;; package.lisp

(defpackage #:psychic-eureka
  (:use #:cl)
  (:export :color :coord :rgb-buffer :*buffer* :*buffer-update-fn*
           :*screen-size-x* :*screen-size-y* 
           :initialize-screen :uninitialize-screen :display-buffer

           :*value-minimum* :*value-maximum* 
           :value-to-grayscale :value-to-heatmap))

