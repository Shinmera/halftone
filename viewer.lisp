#|
This file is a part of halftone
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.halftone)
(in-readtable :qtools)

(define-widget viewer (QGLWidget)
  ((image :initarg :image :accessor image))
  (:default-initargs :image NIL))

(defmethod (setf image) :before (thing (viewer viewer))
  (when (image viewer)
    (finalize (image viewer))
    (setf (slot-value viewer 'image) NIL)))

(defmethod (setf image) ((pathname pathname) (viewer viewer))
  (with-simple-restart (skip "Do not set the image.")
    (setf (image viewer) (load-image pathname)))
  (signal! viewer (do-update)))

(defmethod (setf image) ((null null) (viewer viewer))
  (signal! viewer (do-update)))

(define-signal (viewer do-update) ())

(define-initializer (viewer setup)
  (connect! viewer (do-update) viewer (update)))

(define-override (viewer paint-event) (ev)
  (declare (ignore ev))
  (if image
      (draw-image-fitting image viewer)
      (with-finalizing ((painter (q+:make-qpainter viewer)))
        (q+:erase-rect painter (q+:rect viewer)))))
