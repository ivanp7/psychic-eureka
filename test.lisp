;;;; ll.lisp

(in-package #:psychic-eureka)

(let ((flag 0))
  (setf *buffer* (lambda (x y)
                   (if (= flag (mod (+ x y) 2))
                       (values 255 255 255)
                       (values 0 0 0)))
        *buffer-update-fn* (lambda ()
                             (setf flag (- 1 flag)))))

