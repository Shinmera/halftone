#|
This file is a part of halftone
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.halftone)
(in-readtable :qtools)

(define-widget thumbnail (QWidget)
  ((file :initarg :file :accessor file)
   (selected :initarg :selected :accessor selected))
  (:default-initargs
    :file (error "FILE required.")
    :selected NIL))

(defmethod (setf selected) :after (val (thumbnail thumbnail))
  (q+:repaint thumbnail))

(define-initializer (thumbnail setup)
  (setf (q+:fixed-size thumbnail) (values 128 128)))

(define-subwidget (thumbnail image) NIL
  (with-callback-task result ('thumbnail-loader-task :file file)
    (setf image result)
    (q+:repaint thumbnail)))

(define-override (thumbnail paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter thumbnail)))
    (let ((brush (if selected
                     (q+:highlight (q+:palette thumbnail))
                     (q+:window (q+:palette thumbnail)))))
      (setf (q+:composition-mode painter) (q+:qpainter.composition-mode_source-over))
      (q+:fill-rect painter (q+:rect thumbnail) brush)
      (setf (q+:composition-mode painter) (q+:qpainter.composition-mode_source)))
    (when image
      (let ((target (q+:rect thumbnail)))
        (q+:adjust target 5 5 -5 -5)
        (q+:draw-image painter target image (q+:rect image))))))

(define-override (thumbnail mouse-release-event) (ev)
  (setf (image *main*) file)
  (stop-overriding))

(define-widget gallery (QScrollArea)
  ((location :initarg :location :accessor location))
  (:default-initargs :location (user-homedir-pathname)))

(defmethod (setf location) :after (pathname (gallery gallery))
  (reload-images gallery))

(define-subwidget (gallery scrollable) (q+:make-qwidget))

(define-subwidget (gallery layout) (q+:make-qhboxlayout scrollable)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0))

(define-initializer (gallery setup)
  (setf (q+:background-role gallery) (q+:qpalette.background))
  (setf (q+:vertical-scroll-bar-policy gallery) (q+:qt.scroll-bar-always-off))
  (setf (q+:widget-resizable gallery) T)
  (setf (q+:widget gallery) scrollable))

(define-finalizer (gallery teardown)
  (do-layout (widget layout)
    (finalize widget)))

(defmethod (setf image) ((file pathname) (gallery gallery))
  (do-layout (widget (slot-value gallery 'layout))
    (setf (selected widget) (equalp file (file widget)))))

(defun sort-files (files by &optional descending)
  (macrolet ((sorter (comp key)
               `(lambda (a b) (let ((result (,comp (,key a) (,key b))))
                                (if descending (not result) result)))))
    (sort files (ecase by
                  (:name (sorter string< pathname-name))
                  (:time (sorter uiop:stamp< uiop:safe-file-write-date))))))

(defun directory-images (dir)
  (remove-if-not #'image-file-p (uiop:directory-files dir)))

(defun reload-images (gallery)
  (let ((files (sort-files (directory-images (location gallery)) :time T)))
    (with-slots-bound (gallery gallery)
      (clear-layout layout)
      (dolist (file files)
        (q+:add-widget layout (make-instance 'thumbnail :file file)))
      (setf (image *main*) (first files)))))
