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
              (let ((transform (sb-cga:matrix* xform (ai:transform node))))
                (loop for i across (ai:meshes node)
                   do (mesh-bounds (aref (ai:meshes scene) i) transform))
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

(defparameter *filename* nil)
(defun unload-textures (window)
  (when (and window (scene window))
    (format t "  cleaning up materials~%")
    (loop for i across (ai:materials (scene window))
       for tex.file = (gethash "$tex.file" i)
       do (loop for f in tex.file
             for tex = (getf (cdddr f) :texture-name)
             do (format t "cleaning up old texture ~s~%" f)
             when tex
             do (format t "deleting old texture name ~s~%" tex)
               (gl:delete-textures (list tex))))))

(defun reload-textures (window &optional (clean-up t))
  (when clean-up (unload-textures window))
  (loop for i across (ai:materials (scene window))
     for tex.file = (gethash "$tex.file" i)
     do (loop for tf in tex.file
           for (semantic index file) = tf
           unless (find #\* file)
           do (let* ((cleaned (substitute
                               #\/ #\\
                               (string-trim #(#\space #\Newline
                                              #\Return #\tab) file)))
                     (final-path cleaned))
                ;; some model formats store absolute paths, so
                ;; let's try chopping dirs off the front if we
                ;; can't find it directly
                (loop named outer
                   with (car . d) = (pathname-directory (truename *filename*))
                   for base on (reverse d)
                   for base-path = (make-pathname :directory (cons car (reverse base)))
                   do (format t "base dir ~s~%" base-path)
                   when (probe-file (merge-pathnames cleaned base-path))
                   return  (setf final-path
                                 (cffi-sys:native-namestring
                                  (merge-pathnames cleaned base-path)))
                   do (flet ((relative (f)
                               (cffi-sys:native-namestring
                                (merge-pathnames f base-path))))
                        (loop for offset = 0 then (position #\/ cleaned :start (1+ offset))
                           while offset
                           when (probe-file (relative (subseq cleaned (1+ offset))))
                           do (return-from outer (setf final-path (relative (subseq cleaned (1+ offset))))))))
                (format t "load texture file ~s from ~s~%" file
                        final-path)
                (setf (getf (cdddr tf) :texture-name)
                      (ilut:gl-load-image final-path))))))

(defun reload-scene (&optional window)
  (when *filename*
    (format t "reload scene -> ~s~%" *filename*)
    (let ((s (ai:import-into-lisp
              (cffi-sys:native-namestring (truename *filename*))
              :ai-process-validate-data-structure
              :ai-process-preset-target-realtime-quality)))
      (when (and window s)
        (unload-textures window)
        (setf (scene window) s)
        (format t "  set bounds~%")
        (update-bounds window)
        (format t "  load materials~%")
        (reload-textures window nil))
      (if s
          (format t "loaded ~s~%" *filename*)
          (format t "failed to load ~s~%" *filename*))
      s)))
