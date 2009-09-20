;; misc pieces for samples
(in-package #:classimp-sample)

(defun quat->matrix (q)
  (let ((w (aref q 0))
        (x (aref q 1))
        (y (aref q 2))
        (z (aref q 3)))
    (declare (single-float w x y z))
    (sb-cga:matrix
     (- 1.0 (* 2.0 (+ (expt y 2) (expt z 2))))
     (* 2.0 (+ (* x y) (* z w)))
     (* 2.0 (- (* x z) (* y w)))
     0.0

     (* 2.0 (- (* x y) (* z w)))
     (- 1.0 (* 2.0 (+ (expt x 2) (expt z 2))))
     (* 2.0 (+ (* y z) (* x w)))
     0.0

     (* 2.0 (+ (* x z) (* y w)))
     (* 2.0 (- (* y z) (* x w)))
     (- 1.0 (* 2.0 (+ (expt x 2) (expt y 2))))
     0.0

     0.0 0.0 0.0 1.0)))

(defun nqlerp (a b f)
  (let ((f2 (- 1.0 f)))
    (macrolet ((dim (n)
                 `(+ (* f2 (aref a ,n)) (* f (aref b ,n)))))
      (let* ((r0 (dim 0))
             (r1 (dim 1))
             (r2 (dim 2))
             (r3 (dim 3))
             (l (sqrt (+ (expt r0 2) (expt r1 2) (expt r2 2) (expt r3 2)))))
        (make-array 4 :element-type 'single-float
                    :initial-contents (list (float (/ r0 l) 1f0)
                                            (float (/ r1 l) 1f0)
                                            (float (/ r2 l) 1f0)
                                            (float (/ r3 l) 1f0)))))))

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

(defun axes (s)
  (gl:disable :lighting)
  (gl:with-primitives :lines
    (gl:vertex (- s) 0 0)
    (gl:vertex  s 0 0)
    (gl:vertex 0  (- s) 0)
    (gl:vertex 0  s 0)
    (gl:vertex 0 0 (- s))
    (gl:vertex 0 0  s))
  (gl:enable :lighting))

(defun spin ()
  (float (* 50 (/ (get-internal-real-time)
                  (float internal-time-units-per-second)))))
