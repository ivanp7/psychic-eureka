;;;; test.lisp

(in-package #:psychic-eureka)

(defparameter *color-speed* 10)

(let ((phi0 0))
  (setf *buffer* 
        (flet ((rho (x y)
                 (sqrt (+ (* x x) (* y y))))
               (phi (x y)
                 (atan y x)))
          (lambda (x0 y0)
            (let* ((x (1- (/ (* 2 x0) *screen-size-x*)))
                   (y (1+ (/ (* -2 y0) *screen-size-y*)))
                   (x-leaf x) (y-leaf (+ y 1/2)))
              (let ((rho (rho x y)) 
                    ; (phi (- (/ pi 2) (phi x y)))
                    (rho-leaf (rho x-leaf y-leaf)) 
                    (phi-leaf (- (/ pi 2) (phi x-leaf y-leaf))))
                (flet ((leaf (phi &optional (C 1))
                         (* C (expt (cos (* phi 1/2)) 2) 
                            (+ (* 0.80 (sqrt (abs (cos (* phi 11/2)))))
                               (* 0.15 (expt (cos (* phi 11/2)) 20))
                               (* 0.05 (expt (cos (* phi 11/2)) 1000))))))
                  (let ((d (- rho-leaf (leaf phi-leaf 4/3))))
                    (if (or (minusp d) (and (zerop x-leaf) (minusp y-leaf) 
                                            (> y-leaf -1/4)))
                        (let ((degrees (/ phi-leaf pi 1/180))) 
                          (value-to-circular-heatmap (+ degrees phi0) 
                                                     180 -180))
                        (let* ((glow (expt 0.66 
                                           (* rho 
                                              (+ 7 (* 3 (cos (/ phi0 60)))))))
                               (color (alexandria:clamp (floor (* 255 glow)) 
                                                        0 255))) 
                          (values color color color)))))))))
        *buffer-update-fn* (lambda ()
                             (incf phi0 *color-speed*))))

