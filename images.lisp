#|
This file is a part of halftone
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.halftone)
(in-readtable :qtools)

(defun load-image (pathname)
  (load-image-type (intern (pathname-type pathname) "KEYWORD") pathname))

(defgeneric load-image-type (type pathname)
  (:method (type pathname)
    (let ((image (q+:make-qimage)))
      (unless (q+:load image (uiop:native-namestring pathname))
        (error "Loading image ~s failed!" pathname))
      image)))

(defun load-thumbnail (pathname &optional (size 128))
  (let ((image (q+:make-qimage size size (q+:qimage.format_argb32_premultiplied))))
    (q+:fill image (q+:make-qcolor 0 0 0 0))
    (with-finalizing ((orig (load-image pathname)))
      (draw-image-fitting orig image))))

(defun draw-image-fitting (image target)
  (with-finalizing ((painter (q+:make-qpainter target)))
    (let* ((width (q+:width image))
           (height (q+:height image))
           (aspect (/ width height)))
      (when (< (q+:width target) width)
        (setf width (q+:width target))
        (setf height (/ width aspect)))
      (when (< (q+:height target) height)
        (setf height (q+:height target))
        (setf width (* height aspect)))
      (let ((x (/ (- (q+:width target) width) 2))
            (y (/ (- (q+:height target) height) 2)))
        (q+:draw-image painter
                       (q+:make-qrect (floor x) (floor y) (floor width) (floor height))
                       image
                       (q+:rect image)))))
  target)

(defclass image-loader-task (callback-task)
  ((file :initarg :file :accessor file))
  (:default-initargs :file (error "FILE required.")))

(defmethod print-object ((task image-loader-task) stream)
  (print-unreadable-object (task stream :type T)
    (format stream ":FILE ~s" (file task))))

(defmethod process ((task image-loader-task))
  (load-image (file task)))

(defclass thumbnail-loader-task (image-loader-task)
  ())

(defmethod process ((task thumbnail-loader-task))
  (load-thumbnail (file task)))
