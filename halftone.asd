#|
This file is a part of halftone
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (push :verbose-no-init *features*)
  #+quicklisp (ql:quickload :verbose)
  #-quicklisp (asdf:load-system :verbose))

(asdf:defsystem halftone
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An image viewer using Qtools"
  :homepage "https://github.com/Shinmera/halftone"
  :serial T
  :components ((:file "package")
               (:file "tasks")
               (:file "images")
               (:file "main")
               (:file "viewer")
               (:file "gallery"))
  :defsystem-depends-on (:qtools)
  :depends-on (:qtcore
               :qtgui
               :qtopengl
               :uiop
               :bordeaux-threads)
  :build-operation "qt-program-op"
  :build-pathname "halftone"
  :entry-point "halftone:start")
