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

(defmethod (setf image) ((file pathname) (viewer viewer))
  (with-callback-task (image-loader-task :file file) (result)
    (setf (image viewer) result)
    (signal! viewer (do-update))))

(defmethod (setf image) ((null null) (viewer viewer))
  (signal! viewer (do-update)))

(define-signal (viewer do-update) ())

(define-initializer (viewer setup)
  (connect! viewer (do-update) viewer (update)))

(define-override (viewer paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter viewer)))
    (q+:erase-rect painter (q+:rect viewer)))
  (when image
    (draw-image-fitting image viewer)))
