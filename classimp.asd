;;; -*- mode: lisp; -*-

(defsystem :classimp
  :serial t
  :components ((:file "bindings-package")
               (:file "library")
               (:file "low-level")
               (:file "package")
               (:file "wrappers"))
  :depends-on (:cffi))
