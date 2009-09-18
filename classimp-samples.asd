;;; -*- mode: lisp; -*-

(defsystem :classimp-samples
  :serial t
  :components ((:file "simple-sample")
               )
  :depends-on (:classimp :sb-cga :cl-glut :cl-glu))
