#|
 This file is a part of halftone
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem halftone
  :version "1.1.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An image viewer using Qtools"
  :homepage "https://Shinmera.github.io/halftone/"
  :bug-tracker "https://github.com/Shinmera/halftone/issues"
  :source-control (:git "https://github.com/Shinmera/halftone.git")
  :serial T
  :components ((:file "package")
               (:file "images")
               (:file "main")
               (:file "viewer")
               (:file "gallery"))
  :defsystem-depends-on (:qtools)
  :depends-on (:qtools
               :qtcore
               :qtgui
               :qtopengl
               :uiop
               :verbose
               :simple-tasks
               :bordeaux-threads)
  :build-operation "qt-program-op"
  :build-pathname "halftone"
  :entry-point "halftone:start")
