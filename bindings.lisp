(in-package #:cl-assimp)


(in-package #:cl-opengl-bindings)

(define-foreign-library opengl
  (:darwin (:framework "OpenGL"))
  (:windows "opengl32.dll" :calling-convention :stdcall)
  (:unix (:or "libGL.so" "libGL.so.2" "libGL.so.1")))

(use-foreign-library opengl)
