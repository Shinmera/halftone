#|
This file is a part of halftone
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.halftone)
(in-readtable :qtools)

(defvar *task-controller* NIL)

(defclass task-controller ()
  ((thread :initarg :thread :accessor thread)
   (stack :initarg :stack :accessor stack)
   (lock :initarg :lock :accessor lock)
   (condvar :initarg :condvar :accessor condvar)
   (runnning :initarg :running :accessor running))
  (:default-initargs
   :stack (make-array 0 :adjustable T :fill-pointer 0)
   :lock (bt:make-lock "task-stack")
   :condvar (bt:make-condition-variable :name "task-stack")
   :running NIL))

(defmethod print-object ((controller task-controller) stream)
  (print-unreadable-object (controller stream :type T)
    (format stream "~a :STACK-SIZE ~d"
            (cond ((and (running controller) (thread controller) (bt:thread-alive-p (thread controller))) "RUNNING")
                  ((running controller) "BROKEN")
                  ((and (thread controller) (bt:thread-alive-p (thread controller))) "TERMINATING")
                  (T "")) (length (stack controller)))))

(defun task-loop (controller)
  (with-accessors ((stack stack) (lock lock) (condvar condvar) (running running)) controller
    (bt:acquire-lock lock)
    (loop while running
          do (let ((tasks (make-array (length stack) :initial-contents stack)))
               (setf (fill-pointer stack) 0)
               (bt:release-lock lock)
               (handler-bind ((error (lambda (err)
                                       (v:warn :task-controller "Encountered error: ~a" err)
                                       (invoke-restart 'skip))))
                 (loop for task across tasks
                       do (with-simple-restart (skip "Skip processing ~a" task)
                            (process task)))))
             (bt:acquire-lock lock)
             (when (= 0 (length stack))
               (bt:condition-wait condvar lock)))))

(defgeneric launch (controller)
  (:method ((controller task-controller))
    (bt:with-lock-held ((lock controller))
      (when (and (running controller)
                 (thread controller)
                 (bt:thread-alive-p (thread controller)))
        (error "~s is already running!" controller))
      (setf (thread controller)
            (bt:make-thread (lambda ()
                              (v:debug :task-controller "~s launched." controller)
                              (task-loop controller))
                            :initial-bindings `((*standard-output* . ,*standard-output*)
                                                (*error-output* . ,*error-output*)
                                                (*standard-input* . ,*standard-input*))))
      (setf (running controller) T))
    controller))

(defgeneric shutdown (controller)
  (:method ((controller task-controller))
    (setf (running controller) NIL)
    (bt:condition-notify (condvar controller))
    (when (thread controller)
      (loop while (bt:thread-alive-p (thread controller))))
    (setf (thread controller) NIL)
    controller))

(defun issue-task (task &optional (controller *task-controller*))
  (bt:with-lock-held ((lock controller))
    (vector-push-extend task (stack controller)))
  (bt:condition-notify (condvar controller))
  task)

(defun ensure-controller ()
  (unless *task-controller*
    (launch
     (setf *task-controller* (make-instance 'task-controller)))))

(defclass task ()
  ((done :initarg :done :accessor done)))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type T)
    (format stream "~@[DONE~*~]"
            (done task))))

(defgeneric process (task)
  (:method :before ((task task))
    (v:debug :task-controller "Processing ~a" task))
  (:method :after ((task task))
    (setf (done task) T)))

(defun make-task (name &rest initargs)
  (issue-task (apply #'make-instance name initargs)))

(defclass print-task (task)
  ((message :initarg :message :accessor message))
  (:default-initargs :message (error "MESSAGE required.")))

(defmethod process ((task print-task))
  (format T "~&~a~%" (message task)))

(defclass callback-task (task)
  ((callback :initarg :callback :accessor callback))
  (:default-initargs :callback (error "CALLBACK required.")))

(defmethod process :around ((task callback-task))
  (funcall (callback task) (call-next-method)))

(defmacro with-callback-task (result (name &rest initargs) &body body)
  `(make-task ,name ,@initargs :callback (lambda (,result) ,@body)))
