;; version of simple-sample with some animation, materials
;;
;; (note that the animation and drawing code are very inefficient, so
;;  shouldn't be used directly...)
(in-package #:classimp-sample)

(defclass ai-sample3-window (glut:window)
  ((file :accessor file :initarg :file)
   (scene :accessor scene :initarg :scene)
   (bounds-min :accessor bounds-min)
   (bounds-max :accessor bounds-max)
   (scene-center :accessor scene-center :initform #(0.0 0.0 0.0))
   (scene-scale :accessor scene-scale :initform 1.0)
   (angle :accessor angle :initform 0.0))

  (:default-initargs :width 640 :height 480 :title (lisp-implementation-type)
                     :mode '(:double :rgb :depth :multisample)))

(defmethod update-bounds ((window ai-sample3-window))
  (setf (values (bounds-min window) (bounds-max window))
        (scene-bounds (scene window)))
  (let* ((min (bounds-min window))
         (max (bounds-max window))
         (d (sb-cga:vec- max min))
         (s (/ 1.0 (max (aref d 0) (aref d 1) (aref d 2))))
         (c (sb-cga:vec/ (sb-cga:vec+ min max) 2.0)))
    (format t "bounds = ~s - ~s~%, s=~s c=~s~%" min max s c)
    (setf (scene-scale window) (* 0.95 s))
    (setf (scene-center window) c)))

(defmethod glut:display-window :before ((window ai-sample3-window))
  (il:init)
  (ilut:init)
  (ilut:renderer :opengl)
  (ilut:enable :opengl-conv)
  (update-bounds window)
  (reload-textures window)
  (gl:polygon-mode :front :fill)
  (gl:polygon-mode :back :line)
  (gl:clear-color 0.1 0.1 0.1 1.0)
  (gl:enable :depth-test :multisample))

(defparameter *flip-yz* nil)
(defparameter *y-shift* 0.0)
(defparameter *use-lights* t)
(defparameter *invert-texture-v* nil)
(defparameter *invert-normals* nil)
(defparameter *tris* 0)
(defparameter *spin* t)
(defparameter *anim-speed* 0.8)
(defparameter *wire* t)
(defparameter *dump* nil)
(defparameter *skip-mesh-index* nil)

(defparameter *bone-transforms* nil)
(defparameter *node-transforms* nil)
(defparameter *current-bone-transform* sb-cga:+identity-matrix+)


(defmethod animate-bones ((w ai-sample3-window))
  ;; update node transform (probably should just do this one at load)
  (labels ((nx (n pm)
             (let* ((m (ai:transform n))
                    (pm (sb-cga:matrix* m pm)))
               (setf (gethash (ai:name n) *node-transforms*) pm)
               (setf (gethash (ai:name n) *bone-transforms*) sb-cga:+identity-matrix+)
               (loop for child across (ai:children n)
                     do (nx child pm)))))
    (nx (ai:root-node (scene w)) (sb-cga:identity-matrix)))

  (unless (or (zerop (length (ai:animations (scene w)))))
    (loop
      with anims = (ai:animations (scene w))
      with a = (unless (zerop (length anims)) (aref anims 0))
      with time = (if (and a (not (zerop (ai:duration a))))
                      (mod (* *anim-speed*
                              (/ (get-internal-real-time)
                                 (float internal-time-units-per-second 1f0)))
                           (ai:duration a))
                      0.0)
      for na across (or (ai:channels a)
                        ;; might have mesh-channels or
                        ;; mesh-morph-channels instead, so just
                        ;; pretend we got an empty normal animation
                        #())
      do
         ;; fixme: something should check for anims with no keys at all,
         ;; not sure if that should be here or if we should just drop the
         ;; whole node-anim in translators?
         ;; fixme: clean up interpolation stuff, cache current keyframe, etc
         (let ((r (ai:value (aref (ai:rotation-keys na) 0)))
               (s (aref (ai:scaling-keys na) 0))
               (x (ai:value (aref (ai:position-keys na) 0))))
           (loop
             for i across (ai:rotation-keys na)
             for j from -1
             until (> (ai:key-time i) time)
             do (setf r (ai:value i))
             finally (when (and (>= j 0) (> (ai:key-time i) time))
                       (let ((prev (aref (ai:rotation-keys na) j)))
                         (setf r (nqlerp (ai:value prev) (ai:value i)
                                         (/ (- time (ai:key-time prev))
                                            (- (ai:key-time i)
                                               (ai:key-time prev))))))))
           (loop for i across (ai:scaling-keys na)
                 while (<= (ai:key-time i) time)
                 do (setf s i))
           (loop for i across (ai:position-keys na)
                 for j from -1
                 while (<= (ai:key-time i) time)
                 do (setf x (ai:value i))
                 finally (when (and (>= j 0) (> (ai:key-time i) time))
                           (let* ((prev (aref (ai:position-keys na) j))
                                  (dt (float
                                       (/ (- time (ai:key-time prev))
                                          (- (ai:key-time i) (ai:key-time prev)))
                                       1f0)))
                             (setf x (sb-cga:vec-lerp (ai:value prev) (ai:value i) dt)))))
           (setf (gethash (ai:node-name na) *bone-transforms*)
                 (sb-cga:matrix* (sb-cga:translate x)
                                 (quat->matrix r)
                                 (sb-cga:scale (ai:value s)))))))
  (labels ((ax (n)
             (let* ((m (or (gethash (ai:name n) *bone-transforms*)
                           (gethash (ai:name n) *node-transforms*)))
                    (*current-bone-transform*
                      (sb-cga:matrix* *current-bone-transform* m)))
               (setf (gethash (ai:name n) *bone-transforms*)
                     (if (zerop (length (ai:animations (scene w))))
                         (sb-cga:transpose-matrix
                          (gethash (ai:name n) *node-transforms*))
                         *current-bone-transform*))
               (loop for child across (ai:children n)
                     do (ax child)))))
    (ax (ai:root-node (scene w)))))


(defmethod set-up-material ((w ai-sample3-window) material)
  (flet ((material (param value)
           (when value
             (when (eq param :shininess)
               (setf value (max 0.0 (min 128.0 value))))
             (if (or (numberp value) (= (length value) 4))
                 (gl:material :front param value)
                 (gl:material :front param (map-into (vector 1.0 1.0 1.0 1.0) #'identity value)))
             (when (eq param :diffuse)
               (if (= (length value) 3)
                   (gl:color (aref value 0) (aref value 1) (aref value 2))
                   (gl:color (aref value 0) (aref value 1) (aref value 2) (aref value 3)))))))
    (material :ambient (gethash "$clr.ambient" material))
    (material :diffuse (gethash "$clr.diffuse" material))
    (material :specular (gethash "$clr.specular" material))
    (material :emission (gethash "$clr.emissive" material))
    (material :shininess (gethash "$mat.shininess" material))
    (material :emission (gethash "$clr.emissive" material))
    ;; for gourad shading or 0.0 shininess, turn off specular
    (when (or (eq :ai-shading-mode-gouraud (gethash "$mat.shadingm" material))
              (not (gethash "$mat.shininess" material))
              (zerop (gethash "$mat.shininess" material)))
      (gl:material :front :specular (vector 0.0 0.0 0.0 1.0)))
    (let* ((tex (gethash "$tex.file" material))
           (tex-name (getf (cdddr (assoc :ai-texture-type-diffuse tex))
                           :texture-name)))
      (when *dump* (format t "tex = ~s / ~s~%" tex tex-name))
      (if tex-name
          (progn
            (gl:enable :texture-2d)
            (gl:bind-texture :texture-2d tex-name)
            (gl:tex-parameter :texture-2d :texture-min-filter
                              :linear)
            (gl:tex-parameter :texture-2d :texture-mag-filter
                              :linear)
            (gl:tex-parameter :texture-2d :texture-wrap-s
                              :repeat)
            (gl:tex-parameter :texture-2d :texture-wrap-t
                              :repeat)
            (let ((uvx (car (gethash "$tex.uvtrafo" material))))
              (gl:matrix-mode :texture)
              (gl:load-identity)
              (when *invert-texture-v* (gl:scale 1.0 -1.0 1.0))
              (when uvx
                ;; not sure about order of these...
                (destructuring-bind (type index (x s r)) uvx
                  (declare (ignore type index))
                  (gl:translate (aref x 0) (aref x 1) 0.0)
                  (gl:scale (aref s 0) (aref s 1) 1.0)
                  (gl:translate 0.5 0.5 0.0)
                  (gl:rotate (- (* (/ 180 pi) r)) 0.0 0.0 1.0)
                  (gl:translate -0.5 -0.5 0.0)))
              (gl:matrix-mode :modelview)))
          (gl:disable :texture-2d)))))

(defmethod recursive-render ((w ai-sample3-window))
                                        ;(gl:pop-matrix)
                                        ;(return-from recursive-render nil)
  (labels
      ((r (scene node)
         (gl:with-pushed-matrix
           #++(when (gethash (ai:name node) *node-transforms*)
                (gl:with-pushed-matrix
                  (gl:mult-transpose-matrix (gethash (ai:name node)
                                                     *node-transforms*))
                  (gl:color 0.5 0.4 1.0 1.0)
                  (axes (* (scene-scale w) 0.05))))
           #++(when (gethash (ai:name node) *bone-transforms*)
                (gl:with-pushed-matrix
                  (gl:mult-matrix (gethash (ai:name node) *bone-transforms*))
                  (gl:color 1.5 0.4 0.0 1.0)
                  (axes (* (scene-scale w) 0.1))))
           (loop
             with node-meshes = (ai:meshes node)
             with scene-meshes = (ai:meshes scene)
             for mesh-index across node-meshes
             for mesh = (aref scene-meshes mesh-index)
             for faces = (ai:faces mesh)
             for vertices = (ai:vertices mesh)
             for bones = (ai:bones mesh)
             for normals = (ai:normals mesh)
             when bones
               do (loop
                    with skinned-vertices = (map-into
                                             (make-array (length vertices))
                                             (lambda ()
                                               (sb-cga:vec 0.0 0.0 0.0)))
                    for bone across bones
                    for ofs = (ai:offset-matrix bone)
                    for bx = (gethash (ai:name bone) *bone-transforms*)
                                        ;for bnx = (gethash (ai:name bone) *node-transforms*)
                                        ;for nx = (gethash (ai:name node) *node-transforms*)
                    for mm = (if (and ofs bx)
                                 (sb-cga:matrix*
                                  #++(sb-cga:transpose-matrix (sb-cga:inverse-matrix nx))
                                  bx
                                  (sb-cga:transpose-matrix ofs)
                                  ;;(sb-cga:transpose-matrix bx)
                                  ;;(sb-cga:inverse-matrix (sb-cga:transpose-matrix nx))
                                  )
                                 (or ofs bx))
                    do (when mm
                         (loop for w across (ai:weights bone)
                               for id = (ai:id w)
                               for weight = (ai:weight w)
                               do
                                  (setf (aref skinned-vertices id)
                                        (sb-cga:vec+ (aref skinned-vertices id)
                                                     (sb-cga:vec*
                                                      (sb-cga:transform-point
                                                       (aref vertices id)
                                                       mm)
                                                      weight)))))
                    finally (setf vertices skinned-vertices))
             unless (eql mesh-index *skip-mesh-index*)
               do
                  (gl:material :front :ambient #(0.2 0.2 0.2 1.0))
                  (gl:material :front :diffuse #(0.8 0.8 0.8 1.0))
                  (gl:material :front :emission #(0.0 0.0 0.0 1.0))
                  (gl:material :front :specular #(1.0 0.0 0.0 1.0))
                  (gl:material :front :shininess 15.0)
                  (gl:color 1.0 1.0 1.0 1.0)
                  (when (ai:material-index mesh)
                    (set-up-material w (aref (ai:materials scene)
                                             (ai:material-index mesh))))
                  (gl:with-pushed-matrix
                    (unless bones
                      (gl:mult-transpose-matrix (gethash (ai:name node) *node-transforms*)))
                    (gl:with-primitives
                        (cond
                          ((ai:mesh-has-multiple-primitive-types mesh)
                           (when *dump*
                             (format t "multiple primitive types in mesh?"))
                           (setf normals nil)
                           :points)
                          ((ai:mesh-has-points mesh) (setf normals nil) :points)
                          ((ai:mesh-has-lines mesh) (setf normals nil) :lines)
                          ((ai:mesh-has-triangles mesh) :triangles)
                          ((ai:mesh-has-polygons mesh) :polygons))
                      (loop
                        for face across faces
                        do
                           (incf *tris*)
                           (loop
                             for i across face
                             for v = (aref vertices i)
                             do
                                (when normals
                                  (let ((n (sb-cga:vec* (sb-cga:normalize
                                                         (aref (ai:normals mesh) i))
                                                        (if *invert-normals* -1.0 1.0))))
                                    (gl:normal (aref n 0) (aref n 1) (aref n 2))))
                                (when (and (ai:colors mesh)
                                           (> (length (ai:colors mesh)) 0))
                                  (let ((c (aref (ai:colors mesh) 0)))
                                    (when (setf c (aref c i)))
                                    (when c (gl:color (aref c 0)
                                                      (aref c 1)
                                                      (aref c 2)))))

                                (when (and (ai:texture-coords mesh)
                                           (> (length (ai:texture-coords mesh)) 0))
                                  (let ((tc (aref (ai:texture-coords mesh) 0)))
                                    (when tc
                                      (gl:tex-coord (aref (aref tc i) 0)
                                                    (aref (aref tc i) 1)
                                                    (aref (aref tc i) 2)))))
                                (gl:vertex (aref v 0) (aref v 1) (aref v 2)))))))
           (loop for child across (ai:children node)
                 do (r scene child)))))
    (r (scene w) (ai:root-node (scene w)))))

(defmethod set-up-lights ((w ai-sample3-window))
  (when (and *use-lights* (ai:lights (scene w)))
    ;; fixme: don't disable active lights..
    (gl:disable :light1 :light2 :light3 :light4 :light5 :light6 :light7)
    (loop for light across (ai:lights (scene w))
          for enum in '(:light0 :light1 :light2 :light3
                        :light4 :light5 :light6 :light7)
          do (activate-light enum light (scene-scale w)))))

(defmethod xform ((w ai-sample3-window))
  (if *wire*
      (gl:polygon-mode :front :line)
      (gl:polygon-mode :front :fill))
  (gl:disable :cull-face)
  (gl:enable :normalize :color-material :lighting :light0)
  (gl:color-material :front-and-back :ambient-and-diffuse)
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (glu:look-at 0.0 0.5 3.0
               0.0 0.0 0.0
               0.0 1.0 0.0)

  (unless (and *use-lights* (ai:lights (scene w)))
    (when *dump* (format t "activate default light~%"))
    (gl:disable :light1 :light2 :light3 :light4 :light5 :light6 :light7)
    (gl:enable :light0)
    (gl:light :light0 :position '(10.0 7.0 5.0 0.0))
    (gl:light :light0 :ambient '(0.0 0.0 0.0 1.0))
    (gl:light :light0 :diffuse '(0.8 0.8 0.8 1.0))
    (gl:light :light0 :specular '(0.0 0.0 1.0 1.0))
    (gl:light :light0 :spot-cutoff 180.0))

  (let ((s (scene-scale w))
        (c (scene-center w)))
    (when *spin*
      (if (numberp *spin*)
          (gl:rotate (float *spin* 1.0) 0.0 1.0 0.0)
          (gl:rotate (spin) 0.0 1.0 0.0)))

    (when *flip-yz*
      (gl:rotate (if (numberp *flip-yz*) *flip-yz* -90.0) 1.0 0.0 0.0))
    (gl:translate 0 *y-shift* 0)

    (gl:scale s s s)
    (gl:translate (- (aref c 0)) (- (aref c 1)) (- (aref c 2))))
  (set-up-lights w))

(defparameter *w* nil)
(defmethod glut:display ((w ai-sample3-window))
  (update-fps)
  (setf *w* w)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (with-simple-restart (continue "Continue")
    (when (and (slot-boundp w 'scene)
               (scene w))
      (xform w)
      (let ((*tris* 0))
        (let ((*bone-transforms* (make-hash-table :test 'equal))
              (*node-transforms* (make-hash-table :test 'equal)))
          (if nil
              (progn
                (gl:translate 15 0 -7)
                (loop for j below 5
                      do (gl:translate -30 0 3)
                         (loop for i below 10
                               do (gl:translate 3 0 3)
                                  (animate-bones w)
                                  (recursive-render w))))
              (progn
                (animate-bones w)
                (recursive-render w))))
        (when *dump*
          (setf *dump* nil)
          (format t "scene lights are ~s~%" (if (ai:lights (scene w))
                                                (if *use-lights* "active"
                                                    "disabled")
                                                "unavailable"))
          (format t "drew ~s tris~%" *tris*)))))
  (glut:swap-buffers))

(defmethod glut:idle ((window ai-sample3-window))
  (glut:post-redisplay))

(defmethod glut:reshape ((window ai-sample3-window) width height)
  (setf (glut:width window) width
        (glut:height window) height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((aspect (float (/ height width) 1.0)))
    (gl:ortho -1 1 (- aspect) aspect -10 10))
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :modelview))

(defmethod glut:keyboard ((window ai-sample3-window) key x y)
  (declare (ignore x y))
  ;; fixme: use cond or case or something :p
  (when (eql key #\Esc)
    (glut:destroy-current-window))
  (when (eql key #\d)
    (setf *dump* t))
  (when (eql key #\w)
    (setf *wire* (not *wire*)))
  (when (eql key #\z)
    (setf *flip-yz* (if (numberp *flip-yz*)
                        (mod (+ *flip-yz* 90.0) 360)
                        270.0)))
  (when (eql key #\Z)
    (setf *flip-yz* nil))
  (when (eql key #\?)
    (setf *y-shift* 0.0))
  (when (eql key #\.)
    (incf *y-shift* 0.1))
  (when (eql key #\,)
    (decf *y-shift* 0.1))
  (when (eql key #\i)
    (setf *invert-texture-v* (not *invert-texture-v*)))
  (when (eql key #\I)
    (setf *invert-normals* (not *invert-normals*)))
  (when (eql key #\l)
    (setf *use-lights* (not *use-lights*)))

  (when (eql key #\p)
    (setf *skip-mesh-index* nil)
    (print (nth-value 1 (ignore-errors (next-file-key window -1)))))
  (when (eql key #\n)
    (setf *skip-mesh-index* nil)
    (print (nth-value 1 (ignore-errors (next-file-key window)))))
  (when (eql key #\`)
    (if *skip-mesh-index*
        (incf *skip-mesh-index*)
        (setf *skip-mesh-index* 0)))

  (when (eql key #\space)
    (setf *skip-mesh-index* nil)
    (with-simple-restart (continue "continue")
      (reload-scene window)))

  (when (eql key #\Backspace)
    (setf *anim-speed* 1.0))
  (when (or (eql key #\+) (eql key #\=))
    (setf *anim-speed* (* *anim-speed* 1.1)))
  (when (eql key #\-)
    (setf *anim-speed* (/ *anim-speed* 1.1)))

  (when (eql key #\r)
    (setf *spin* (if (numberp *spin*) nil (if *spin* (spin) t))))
  (when (eql key #\[)
    (setf *spin* (if (numberp *spin*)
                     (1- *spin*)
                     (spin))))
  (when (eql key #\])
    (setf *spin* (if (numberp *spin*)
                     (1+ *spin*)
                     (spin))))
  (when (eql key #\v)
    (format t "using ai ~s . ~s.~% extensions supported: ~s~% ~s~%"
            (%ai:ai-get-version-major)
            (%ai:ai-get-version-minor)
            (ai::get-extension-list)
            (%ai:ai-get-legal-string))))

(defun ai-sample3 (&optional (file (merge-pathnames
                                    "cube.dae" #.(or *compile-file-pathname*
                                                     *load-pathname*))))
  (format t "loading ~s (~s) ~%" file (probe-file file))
  (ai:with-log-to-stdout ()
    (let* ((*filename*  (namestring (truename file)))
           (scene (reload-scene)))
      (when scene (glut:display-window (make-instance 'ai-sample3-window
                                                      :scene scene))))))

;(ai-sample3)
;(glut:show-window)
;(ai-sample3 (cffi-sys:native-namestring (merge-pathnames "src/assimp/test/models/B3D/dwarf2.b3d" (user-homedir-pathname))))
;(cl-glut:main-loop)

;(find-files (merge-pathnames "src/assimp/test/models-nonbsd/AMF/" (user-homedir-pathname)))
;(find-files (merge-pathnames "src/assimp/test/models/MDL/" (user-homedir-pathname)))

;(ai-sample3 "/tmp/JaiquaFromXSI.dae")

;(ai-sample3 "/tmp/cube.dae")
(setf ai::*translate-verbose* t)
#++
(ql:quickload 'classimp-samples)
#++
(loop for i across glut::*id->window*
      when i do (glut:set-window (glut::id i)) (glut:show-window))
