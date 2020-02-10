;;;; test.lisp

(in-package #:psychic-eureka)

(let ((flag 0))
  (setf *buffer* (lambda (x y)
                   (value-to-heatmap (+ *value-minimum*
                                      (* (- *value-maximum* *value-minimum*)
                                         (/ (+ x y) (+ *screen-size-x* 
                                                       *screen-size-y*))))))
        *buffer-update-fn* (lambda ()
                             (setf flag (- 1 flag)))))

