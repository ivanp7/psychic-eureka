;;;; heatmap.lisp

(in-package #:psychic-eureka)

(defvar *value-minimum* -100)
(defvar *value-maximum* 100)

(defun value-to-grayscale (value &optional value-max value-min)
  (let ((value-max (or value-max *value-maximum*))
        (value-min (or value-min *value-minimum*)))
    (let ((index (alexandria:clamp (/ (- value value-min) 
                                      (- value-max value-min)) 0 1)))
      (let ((color (floor (* index 255))))
        (values color color color)))))

;; violet -> minimum
;; blue
;; green -> middle
;; yellow
;; red -> maximum
(defun value-to-heatmap (value &optional value-max value-min)
  (let ((value-max (or value-max *value-maximum*))
        (value-min (or value-min *value-minimum*)))
    (let ((index (alexandria:clamp (/ (- value value-min) 
                                      (- value-max value-min)) 0 1)))
      (cond
        ((<= index 1/5)
         (let ((color (floor (* (+ (* 5/2 index) 1/2) 255))))
           (values (- 255 color) 0 color)))
        ((<= index 3/5)
         (let ((color (floor (* 5/2 (- index 1/5) 255))))
           (values 0 color (- 255 color))))
        (t (let ((color (floor (* 5/2 (- index 3/5) 255))))
             (values color (- 255 color) 0)))))))

