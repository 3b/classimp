;; misc pieces for samples
(in-package #:classimp-sample)

(defun quat->matrix (q)
  (let ((w (- (aref q 0)))
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
    ;; make sure we get shortest path between orientations
    ;; (if (a dot b) < 0, negate b)
    (let ((d (+ (* (aref a 0) (aref b 0))
                (* (aref a 1) (aref b 1))
                (* (aref a 2) (aref b 2))
                (* (aref a 3) (aref b 3)))))
      (when (< d 0)
        (map-into b #'- b)))
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

(defun v3->v4 (v &optional w)
  (vector (aref v 0) (aref v 1) (aref v 2) w))

(defun activate-light (enum light &optional (s 1.0))
  (gl:enable enum)
  (destructuring-bind (type &key name direction position
                            diffuse specular ambient
                            inner-angle outer-angle) light
    (declare (ignore name inner-angle))
    (gl:light enum :ambient (v3->v4 ambient 1.0))
    (gl:light enum :diffuse (v3->v4 diffuse 1.0))
    (gl:light enum :specular (v3->v4 specular 1.0))
    (case type
      (:ai-light-source-directional
       (gl:light enum :position (v3->v4 (sb-cga:vec* direction -1.0) 0.0)))
      (:ai-light-source-point
       (gl:light enum :position (v3->v4 position 1.0)))
      (:ai-light-source-spot
       (gl:light enum :position (v3->v4 position 1.0))
       (gl:light enum :spot-direction direction)
       ;; fixme: use inner-angle to calculate :spot-exponent
       (gl:light enum :spot-cutoff (* outer-angle (/ 180.0 pi)))
       (gl:light enum :spot-direction direction)))))


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
           if (and (plusp (length file))
                   (char= #\* (char file 0))) ;; embedded file
           do (let ((tex-index (parse-integer (subseq file 1) :junk-allowed t)))
                (if (or (not tex-index) (not (numberp tex-index))
                        (minusp tex-index)
                        (>= tex-index (length (ai:textures (scene window)))))
                    (format t "bad embedded texture index ~s (~s)~%"
                            tex-index (subseq file 1))
                    (destructuring-bind (type w h data &optional format-hint)
                        (aref (ai:textures (scene window)) tex-index )
                      (declare (ignorable format-hint))
                      (if (eq type :bgra)
                          (let ((name (car (gl:gen-textures 1))))
                            (format t "load embedded texture ~s x ~s~%" w h)
                            (gl:bind-texture :texture-2d name)
                            (gl:tex-parameter :texture-2d :texture-min-filter
                                              :linear)
                            (gl:tex-parameter :texture-2d :generate-mipmap t)
                            (gl:tex-parameter :texture-2d :texture-min-filter
                                              :linear-mipmap-linear)
                            (gl:tex-image-2d :texture-2d 0 :rgba w h
                                             0 :bgra :unsigned-byte
                                             data)
                            (setf (getf (cdddr tf) :texture-name) name))
                          (let ((iname (il:gen-image))
                                (result nil))
                            (il:with-bound-image iname
                              (cffi:with-pointer-to-vector-data (p data)
                                (il:load-l :unknown p (length data) ))
                              (setf result (ilut:gl-bind-tex-image)))
                            (il:delete-images (list iname))
                            (setf (getf (cdddr tf) :texture-name) result))))
))
           else if (find #\* file) ;; texture name with * in it?
           do (format t "bad texture name ~s~%" file)
           else ;; external texture
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
                   ;;do (format t "base dir ~s~%" base-path)
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
              :processing-flags '(:ai-process-validate-data-structure
                                  :ai-process-preset-target-realtime-quality))))
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

(defparameter *frame-count* 0)
(defparameter *last-fps-message-time* 0)
(defparameter *last-fps-message-frame-count* 0)
(defparameter *fps-message-interval* 2.000) ;; in second

(defun update-fps ()
  ;; update the frame count
  (incf *frame-count*)
  ;; handle tick count wrapping to 0
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    (when (< now *last-fps-message-time*)
     (setf *last-fps-message-time* now))
    ;; see if it is time for next message
    (when (>= now (+ *last-fps-message-time* *fps-message-interval*))
      (let ((frames (- *frame-count* *last-fps-message-frame-count*))
            (seconds (- now *last-fps-message-time*)))
        (format t "~s seconds: ~s fps, ~s ms per frame~%"
                (float seconds)
               (if (zerop seconds) "<infinite>" (float (/ frames seconds)))
               (if (zerop frames) "<infinite>" (float (/ seconds frames)))))
     (setf *last-fps-message-time* now)
     (setf *last-fps-message-frame-count* *frame-count*))))
