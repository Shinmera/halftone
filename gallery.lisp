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

(define-signal (thumbnail do-update) ())

(defmethod (setf selected) :after (val (thumbnail thumbnail))
  (signal! thumbnail (do-update)))

(define-initializer (thumbnail setup)
  (setf (q+:fixed-size thumbnail) (values 128 128))
  (connect! thumbnail (do-update) thumbnail (update)))

(define-subwidget (thumbnail image) NIL
  (with-callback-task result ('thumbnail-loader-task :file file)
    (setf image result)
    (signal! thumbnail (do-update))))

(define-override (thumbnail paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter thumbnail)))
    (let ((brush (if selected
                     (q+:highlight (q+:palette thumbnail))
                     (q+:window (q+:palette thumbnail)))))
      (q+:fill-rect painter (q+:rect thumbnail) brush))
    (when image
      (let ((target (q+:rect thumbnail)))
        (q+:adjust target 5 5 -5 -5)
        (q+:draw-image painter target image (q+:rect image))))))

(define-override (thumbnail mouse-release-event) (ev)
  (when (= (enum-value (q+:button ev)) (q+:qt.left-button))
    (setf (image *main*) file))
  (stop-overriding))

(define-widget gallery (QScrollArea)
  ((location :initarg :location :accessor location)
   (thumbnails :accessor thumbnails)
   (current :initform -1 :accessor current))
  (:default-initargs :location (user-homedir-pathname)))

(defmethod (setf location) :after (pathname (gallery gallery))
  (reload-images gallery))

(defmethod (setf current) :around (num (gallery gallery))
  (with-slots-bound (gallery gallery)
    (when (and (/= current num)
               (< -1 num (length thumbnails)))
      (when (/= current -1)
        (setf (selected (elt thumbnails current)) NIL))
      (call-next-method)
      (let ((thumbnail (elt thumbnails current)))
        (setf (selected thumbnail) T)
        (setf (image *main*) (file thumbnail))
        (q+:ensure-widget-visible gallery thumbnail))))
  num)

(defmethod (setf image) ((file pathname) (gallery gallery))
  (loop for i from 0
        for widget across (slot-value gallery 'thumbnails)
        do (when (equalp file (file widget))
             (setf (current gallery) i))))

(define-subwidget (gallery scrollable) (q+:make-qwidget))

(define-subwidget (gallery layout) (q+:make-qhboxlayout scrollable)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0))

(defmacro keycase (key &body cases)
  (let ((k (gensym "KEY")))
    `(let ((,k ,key))
       (cond ,@(loop for case in cases
                     collect `((= ,k ,(first case))
                               ,@(rest case)))))))

(define-override (gallery key-release-event) (ev)
  (flet ((setc (n) (setf (current gallery) n)))
    (keycase (q+:key ev)
      ((q+:qt.key_d)
       (setc (1+ (current gallery))))
      ((q+:qt.key_a)
       (setc (1- (current gallery))))
      ((q+:qt.key_right)
       (setc (1+ (current gallery))))
      ((q+:qt.key_left)
       (setc (1- (current gallery))))
      ((q+:qt.key_page-up)
       (setc (1- (length thumbnails))))
      ((q+:qt.key_page-down)
       (setc 0)))))

(define-override (gallery resize-event) (ev)
  (cond ((and (< (+ 10 (q+:width gallery)) (q+:height gallery))
              (qtypep layout (find-qclass "QHBoxLayout")))
         (change-layout gallery :vertical))
        ((and (< (+ 10 (q+:height gallery)) (q+:width gallery))
              (qtypep layout (find-qclass "QVBoxLayout")))
         (change-layout gallery :horizontal)))
  (stop-overriding))

(define-initializer (gallery setup)
  (setf (q+:background-role gallery) (q+:qpalette.background))
  (setf (q+:vertical-scroll-bar-policy gallery) (q+:qt.scroll-bar-always-off))
  (setf (q+:horizontal-scroll-bar-policy gallery) (q+:qt.scroll-bar-always-on))
  (setf (q+:widget-resizable gallery) NIL)
  (setf (q+:widget gallery) scrollable))

(define-finalizer (gallery teardown)
  (do-layout (widget layout)
    (finalize widget)))

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
      (setf thumbnails (make-array 0 :adjustable T :fill-pointer 0))
      (setf current -1)
      (dolist (file files)
        (let ((thumb (make-instance 'thumbnail :file file)))
          (vector-push-extend thumb (thumbnails gallery))
          (q+:add-widget layout thumb)))
      (if (qtypep layout (find-qclass "QHBoxLayout"))
          (setf (q+:fixed-size scrollable) (values (* 128 (length (thumbnails gallery))) 128))
          (setf (q+:fixed-size scrollable) (values 128 (* 128 (length (thumbnails gallery))))))
      (setf (image *main*) (first files)))))

(defun change-layout (gallery direction)
  (with-slots-bound (gallery gallery)
    (let* ((newwidget (q+:make-qwidget))
           (newlayout (ecase direction
                        (:horizontal
                         (setf (q+:vertical-scroll-bar-policy gallery) (q+:qt.scroll-bar-always-off))
                         (setf (q+:horizontal-scroll-bar-policy gallery) (q+:qt.scroll-bar-always-on))
                         (q+:make-qhboxlayout newwidget))
                        (:vertical 
                         (setf (q+:vertical-scroll-bar-policy gallery) (q+:qt.scroll-bar-always-on))
                         (setf (q+:horizontal-scroll-bar-policy gallery) (q+:qt.scroll-bar-always-off))
                         (q+:make-qvboxlayout newwidget))))
           (oldwidget scrollable)
           (oldlayout layout))
      (setf (q+:margin newlayout) 0)
      (setf (q+:spacing newlayout) 0)
      (setf layout newlayout)
      (setf scrollable newwidget)
      (setf (q+:widget gallery) scrollable)
      (finalize oldlayout)
      (finalize oldwidget)
      (reload-images gallery))))
