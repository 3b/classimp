;;; -*- mode: lisp; -*-

(defsystem :classimp-samples
  :serial t
  :components ((:file "simple-sample")
               (:file "classimp-sample-package")
               (:file "sample-utils")
               (:file "simple-sample2")
               )
  :depends-on (:classimp :sb-cga :cl-glut :cl-glu))
