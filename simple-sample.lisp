;; port of assimp Sample_SimpleOpenGL.c to cl-opengl/cl-glut
;;
(defpackage #:simple-sample
  (:use :cl)
  (:export #:ai-sample))
(in-package #:simple-sample)

(defclass ai-sample-window (glut:window)
  ((file :accessor file :initarg :file)
   (scene :accessor scene :initarg :scene)
   (bounds-min :accessor bounds-min)
   (bounds-max :accessor bounds-max)
   (angle :accessor angle :initform 0.0))

  (:default-initargs :width 640 :height 480 :title "..."
                     :mode '(:double :rgb :depth :multisample)))

(defun scene-bounds (scene)
  (let ((min (sb-cga:vec 1f10 1f10 1f10))
        (max (sb-cga:vec -1f10 -1f10 -1f10)))
    (labels ((mesh-bounds (mesh xform)
               (loop for vertex across (ai:vertices mesh)
                  for transformed = (sb-cga:transform-point
                                     vertex
                                     (sb-cga:transpose-matrix xform))
                  do (setf min (sb-cga:vec-min min transformed)
                           max (sb-cga:vec-max max transformed))))
            (node-bounds (node xform)
              (let ((transform (sb-cga:matrix* (ai:transform node) xform)))
                (loop for i across (ai:meshes node)
                   do (mesh-bounds (aref (ai:meshes scene) i) xform))
                (loop for i across (ai:children node)
                   do (node-bounds i transform)))))
      (node-bounds (ai:root-node scene) (sb-cga:identity-matrix)))
    (values min max)))

(defmethod glut:display-window :before ((window ai-sample-window))
 (format t "display-window :before ai-sample-window...~%")
  (setf (values (bounds-min window) (bounds-max window))
        (scene-bounds (scene window)))
  (gl:polygon-mode :front :line)
  (gl:polygon-mode :back :line)
  (gl:clear-color 0.1 0.1 0.1 1.0)

  (gl:enable :depth-test :multisample))

(defparameter *tris* 0)
(defparameter *spin* t)
(defparameter *dump* nil)

(defun recursive-render (scene node)
  (let ((m (ai:transform node)))
    (gl:mult-transpose-matrix m)
    (loop
       with node-meshes = (ai:meshes node)
       with scene-meshes = (ai:meshes scene)
       for mesh-index across node-meshes
       for mesh = (aref scene-meshes mesh-index)
       for faces = (ai:faces mesh)
       for vertices = (ai:vertices mesh)
       do
       (gl:with-primitives :triangles
         (gl:color 0.0 1.0 0.0 1.0)
         (loop
            for face across faces
            do
            (incf *tris*)
            (loop
               for i across face
               for v = (aref vertices i)
               do (gl:vertex (aref v 0) (aref v 1) (aref v 2)))))))
  (loop for child across (ai:children node)
     do (gl:with-pushed-matrix
          (recursive-render scene child))))

(defun xform (w)
  (gl:polygon-mode :front :line)
  (gl:disable :cull-face)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glu:look-at 0.0 0.0 3.0
               0.0 0.0 0.0
               0.0 1.0 0.0)
  (let* ((min (bounds-min w))
         (max (bounds-max w))
         (d (sb-cga:vec- max min))
         (s (/ 1.0 (max (aref d 0) (aref d 1) (aref d 2))))
         (c (sb-cga:vec/ (sb-cga:vec+ min max) 2.0)))
    (when *spin*
      (gl:rotate (float (* 50 (/ (get-internal-real-time) (float internal-time-units-per-second)))) 0.0 1.0 0.0))
    (gl:scale s s s)
    (gl:translate (- (aref c 0)) (- (aref c 1)) (- (aref c 2)))))

(defmethod glut:display ((w ai-sample-window))
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (when (and (slot-boundp w 'scene)
             (scene w))
    (let ((*tris* 0))

      (xform w)
      (recursive-render (scene w) (ai:root-node (scene w)))
      (when *dump*
       (setf *dump* nil)
       (format t "drew ~s tris~%" *tris*))))

  (glut:swap-buffers))

(defmethod glut:idle ((window ai-sample-window))
  (glut:post-redisplay))

(defun resize-window (w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -1 1 -1 1 -10 10)
  (gl:viewport 0 0 w h))

(defmethod glut:reshape ((window ai-sample-window) width height)
  (setf (glut:width window) width
        (glut:height window) height)
  (resize-window width height))

(defmethod glut:keyboard ((window ai-sample-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window))
  (when (eql key #\r)
    (setf *spin* (not *spin*)))
  (when (eql key #\d)
    (setf *dump* t)))

(defun ai-sample (&optional (file (merge-pathnames
                                   "cube.dae" #.(or *compile-file-pathname*
                                                    *load-pathname*))))
  (format t "loading ~s (~s) ~%" file
          (probe-file file))
  (ai:with-log-to-stdout ()
    (let ((scene
           (ai:import-into-lisp (cffi-sys:native-namestring (truename file))
                                :ai-process-preset-target-realtime-quality)))
      (when scene (glut:display-window (make-instance 'ai-sample-window
                                                      :scene scene))))))

;(ai-sample)
;(cl-glut:main-loop)