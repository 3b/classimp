;;; -*- mode: lisp; -*-

(defsystem :classimp
  :description "Common Lisp/CFFI bindings for Open Asset Import Library (http://assimp.sourceforge.net/)"
  :serial t
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :components ((:file "bindings-package")
               (:file "library")
               (:file "low-level")
               (:file "package")
               (:file "utils")
               (:file "config")
               (:file "wrappers"))
  :depends-on (:cffi :split-sequence))
