;;;; ll.lisp

(in-package #:psychic-eureka)

(deftype color () '(unsigned-byte 8))
(deftype coord () 'fixnum)
(deftype rgb-buffer () '(function (coord coord) (values color color color)))

(defvar *buffer* nil)
(defvar *buffer-update-fn* (constantly nil))

(defun initialize-screen (&optional (stream *standard-output*))
  (format stream "~C[?25l~C[?1049h" #\Esc #\Esc)
  (force-output stream)
  t)

(defun uninitialize-screen (&optional (stream *standard-output*))
  (format stream "~C[?25h~C[?1049l" #\Esc #\Esc)
  (force-output stream)
  t)

(let ((byte-to-string 
        (make-array 256 :element-type 'string
                    :initial-contents
                    (loop :for value :below 256
                          :collect (format nil "~A" value))))) 

  (defun display-buffer (buffer size-x size-y
                        &key (stream *standard-output*) (displ-x 0) (displ-y 0)
                        init-sequence linefeed)
    "Print buffer to STREAM using ANSI color codes.
    Custom initializing sequence and line feed may be provided."
    (declare (type rgb-buffer buffer) 
             (type coord size-x size-y displ-x displ-y) 
             (type stream stream)
             (type (or null string) init-sequence linefeed))
    (let ((init-sequence (or init-sequence 
                             (format nil "~C[~A;~AH" 
                                     #\Esc (1+ displ-y) (1+ displ-x))))
          (linefeed (or linefeed (format nil "~C[0m~%" #\Esc))))
      (write-string init-sequence stream)
      (dotimes (y size-y)
        (declare (type coord y))
        (dotimes (x size-x)
          (declare (type coord x))
          (multiple-value-bind (bg-red bg-green bg-blue) 
              (funcall buffer x (* 2 y))
            (multiple-value-bind (fg-red fg-green fg-blue)
                (funcall buffer x (1+ (* 2 y)))
              (write-char #\Esc stream)
              (write-string "[38;2;")
              (write-string (aref byte-to-string fg-red) stream)
              (write-char #\; stream)
              (write-string (aref byte-to-string fg-green) stream)
              (write-char #\; stream)
              (write-string (aref byte-to-string fg-blue) stream)
              (write-char #\m stream)
              (write-char #\Esc stream)
              (write-string "[48;2;")
              (write-string (aref byte-to-string bg-red) stream)
              (write-char #\; stream)
              (write-string (aref byte-to-string bg-green) stream)
              (write-char #\; stream)
              (write-string (aref byte-to-string bg-blue) stream)
              (write-char #\m stream)
              (write-char #\▄))))
        (write-string linefeed stream)))
    (force-output stream)
    t))

