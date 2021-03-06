#|
This file is a part of halftone
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.halftone)
(in-readtable :qtools)

(defvar *main*)

(define-widget dock-container (QDockWidget)
  ((widget :initarg :widget :reader widget)
   (title :initarg :title :reader title))
  (:default-initargs
    :widget (error "WIDGET required.")
    :title ""))

(define-initializer (dock-container setup)
  (setf (q+:widget dock-container) widget)
  (setf (q+:window-title dock-container) title)
  (setf (q+:features dock-container) (q+:qdockwidget.dock-widget-movable)))

(define-widget main-window (QMainWindow)
  ())

(define-initializer (main-window set-main 100)
  (setf *main* main-window)
  (setf (q+:window-title main-window) "Halftone")
  (q+:resize main-window 800 600))

(define-subwidget (main-window viewer) (make-instance 'viewer)
  (setf (q+:central-widget main-window) viewer))

(define-subwidget (main-window gallery) (make-instance 'gallery))

(define-subwidget (main-window dockable) (make-instance 'dock-container :widget gallery :title "Gallery")
  (q+:add-dock-widget main-window (q+:qt.bottom-dock-widget-area) dockable))

(defgeneric (setf image) (image thing)
  (:method (thing (main main-window))
    (with-slots-bound (main main-window)
      (setf (image viewer) thing)
      (setf (image gallery) thing))))

(define-menu (main-window File)
  (:item ("Open" (ctrl o))
         (let ((dir (q+:qfiledialog-get-existing-directory main-window "Browse" (uiop:native-namestring (location gallery)))))
           (unless (or (qt:null-qobject-p dir) (string= dir ""))
             (setf (location gallery) (uiop:parse-native-namestring dir :ensure-directory T)))))
  (:separator)
  (:item ("Quit" (ctrl q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (let ((system (asdf:find-system :halftone)))
           (with-finalizing ((box (q+:make-qmessagebox main-window)))
             (setf (q+:window-title box) "About Halftone")
             (setf (q+:text box) (format NIL "~a<br />
The source code is openly available and licensed under ~a.<br />
<br />
Homepage: <a href=\"~a~:*\">~a</a><br />
Author: ~a<br />
Version: ~a"
                                         (asdf:system-description system)
                                         (asdf:system-license system)
                                         (asdf:system-homepage system)
                                         (asdf:system-author system)
                                         (asdf:component-version system)))
             (#_exec box)))))

(defun main ()
  (unwind-protect
       (progn
         (bt:make-thread (lambda () (simple-tasks:start-runner *image-runner*)))
         (with-main-window (window 'main-window :name "Halftone")
           (with-slots-bound (window main-window)
             (setf (location gallery) (user-homedir-pathname)))))
    (simple-tasks:stop-runner *image-runner*)))

(defun start ()
  #+:sbcl (sb-ext:disable-debugger)
  (setf v:*global-controller* (v:make-standard-global-controller))
  (let ((*main* NIL))
    (main)))

(deploy:define-hook (:deploy stop-verbose) ()
  (v:remove-global-controller))
