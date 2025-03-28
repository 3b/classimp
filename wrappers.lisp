(in-package #:classimp)

;; try to notice is someone recompiled low-level with restart due to
;; shared lib version mismatch and this didn't get recompiled
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *wrapper-version* #.%ai::*version*)
  (eval-when (:compile-toplevel)
    (setf *wrapper-version* %ai::*version*))
  (eval-when (:load-toplevel :execute)
    (unless (eql *wrapper-version* %ai::*version*)
      (error "classimp wrappers and bindings were compiled with ~
              different version of assimp?~%~
              wrappers = ~a, bindings =  ~a"
             *wrapper-version* %ai::*version*))))

(defparameter *loader-default-animation-ticks-per-second* 24.0)
(defparameter *loader-translate-times* t)
(defparameter *translate-verbose* nil)

(defclass aabb ()
  ((mmin :accessor mmin :initarg mmin)
   (mmax :accessor mmax :initarg mmax)))

(defclass node ()
  ((name :initarg name :accessor name :initform "")
   (transform :initarg transform :accessor transform)
   (parent :initarg parent :accessor parent :initform nil)
   ;; not sure if children should be an array or list, probably array for now
   (children :initarg children :accessor children :initform nil)
   ;; array of indices into scene mesh array
   (meshes :initarg meshes :accessor meshes :initform nil)
   (metadata :initarg metadata :accessor metadata :initform nil)))

(defclass scene ()
  ;; flags is raw uint, use accessors defined below
  ((flags :initarg flags :initform 0 :accessor %flags)
   (root-node :initarg root :accessor root-node)
   ;; array of actual meshes, possibly should rename this or node::meshes?
   (meshes :initarg meshes :accessor meshes :initform nil)
   (materials :initarg materials :accessor materials :initform nil)
   (animations :initarg animations :accessor animations :initform nil)
   (textures :initarg textures :accessor textures :initform nil)
   (lights :initarg lights :accessor lights :initform nil)
   (cameras :initarg cameras :accessor cameras :initform nil)
   ;; 5.0+
   (metadata :initarg metadata :accessor metadata :initform nil)
   ;; 5.1+
   (name :initarg name :accessor name :initform nil)))

;; todo: do these need writers?
(defmethod scene-incomplete-p ((scene scene))
  (logbitp 0 (%flags scene)))
(defmethod scene-validated-p ((scene scene))
  (logbitp 1 (%flags scene)))
(defmethod scene-validation-warning-p ((scene scene))
  (logbitp 2 (%flags scene)))
(defmethod scene-non-verbose-p ((scene scene))
  (logbitp 3 (%flags scene)))
(defmethod scene-terrain-p ((scene scene))
  (logbitp 4 (%flags scene)))
(defmethod scene-allow-shared-p ((scene scene))
  (logbitp 5 (%flags scene)))



;; just using an array of uint for face for now...
;;(defclass face ())

;; not sure if this could be handled better somewhere else or not...
(defclass vertex-weight ()
  ((id :accessor id :initarg id)
   (weight :accessor weight :initarg weight)))

(defclass bone ()
  ;; fixme: should probably add a link to the actual node here, and
  ;; look it up on load instead of just having the name of the node
  ((name :accessor name :initarg name :initform "")
   ;; array of vertex-weight
   (weights :accessor weights :initarg weights)
   (offset-matrix :accessor offset-matrix :initarg offset-matrix)))

(defclass anim-mesh ()
  ((name :accessor name :initarg name)
   (vertices :accessor vertices :initarg vertices)
   (normals :accessor normals :initarg normals)
   (tangents :accessor tangents :initarg tangents)
   (bitangents :accessor bitangents :initarg bitangents)
   (colors :accessor colors :initarg colors)
   (texture-coords :accessor texture-coords :initarg texture-coords)
   (weight :accessor weight :initarg weight)))

(defclass mesh ()
  ;; uint containing 1 or more of ai-primitive-type enum/bitfield values
  ;; use accessors below...
  ((primitive-types :accessor primitive-types :initarg primitive-types)
   ;; arrays of 3d vectors
   (vertices :accessor vertices :initarg vertices)
   ;; optional, array of 3d vectors, unused elements may be NaN
   ;; (ex. for verts used only by lines/points if mixed)
   (normals :accessor normals :initarg normals :initform nil)
   ;; optional, ...
   (tangents :accessor tangents :initarg tangents :initform nil)
   (bitangents :accessor bitangents :initarg bitangents :initform nil)
   ;; optional,
   ;; array of arrays of rgba float colors
   ;; (aref (colors ...) 0) is array of (length vertices) rgba colors,
   ;; (length (colors ...)) is number of colors per vertex
   ;; each color is a (optional) vector of 4 single floats
   ;; (may actually get a 0-length array instead of nil if no colors present
   ;;  not sure which is better)
   (colors :accessor colors :initarg colors :initform nil)
   ;; optional array of 4 optional arrays of 3d vectors, same layout as colors
   (texture-coords :accessor texture-coords :initarg texture-coords :initform nil)
   ;; # of valid components in values in corresponding array in
   ;; texture-coords, see aiMesh.h
   ;; (possibly should actually drop the unused data in texture-coords as well?
   ;;  though since we would most likely want to keep this around anyway
   ;;  for ease of using the data, not worrying about it for now...)
   (components-per-texture-coord :accessor components-per-texture-coord
                                 :initarg components-per-texture-coord
                                 :initarg nil)
   ;; array of arrays of indices into vertex data
   ;; 1 array of indices per face (even for points, for now)
   (faces :accessor faces :initarg faces)
   ;;
   (bones :accessor bones :initarg bones :initform nil)
   ;; index into scene material list
   ;; fixme: probably should dereference material and store it here directly?
   (material-index :accessor material-index :initarg material-index)
   ;; rest are optional depending on library version, so might be NIL
   (name :accessor name :initform nil :initarg name)
   (anim-meshes :accessor anim-meshes :initform nil :initarg anim-meshes)
   (morphing-method :accessor morphing-method :initform nil :initarg morphing-method)
   (aabb :accessor aabb :initform nil :initarg aabb)
   (texture-coord-names :accessor texture-coord-names :initform nil
                        :initarg texture-coord-names)))

;; fixme: should probably use the enum/bitfields instead of hard coding #s here
(defmethod mesh-has-points ((mesh mesh))
  (logbitp 0 (primitive-types mesh)))
(defmethod mesh-has-lines ((mesh mesh))
  (logbitp 1 (primitive-types mesh)))
(defmethod mesh-has-triangles ((mesh mesh))
  (logbitp 2 (primitive-types mesh)))
(defmethod mesh-has-polygons ((mesh mesh)) ;; including quads
  (logbitp 3 (primitive-types mesh)))
(defmethod mesh-has-multiple-primitive-types ((mesh mesh))
  (/= 1 (logcount (primitive-types mesh))))



(defclass animation ()
  ((name :accessor name :initarg name)
   (duration :initarg duration :accessor duration)
   (ticks-per-second :accessor ticks-per-second :initarg ticks-per-second)
   (channels :accessor channels :initarg channels)
   ;; hash of node name -> node-animation instances for this anim
   (index :accessor index :initarg index)
   ;; optional depending on version, might be NIL
   (mesh-channels :accessor mesh-channels :initarg mesh-channels :initform nil)
   (mesh-morph-channels :accessor mesh-morph-channels
                        :initarg mesh-morph-channels :initform nil)))

;; possibly should combine these, or enforce type for value slot
(defclass vector-key ()
  ;; time stored in seconds if possible
  ((time :accessor key-time :initarg time) ;; double
   ;; array of 3 floats
   (value :accessor value :initarg value)
   ;; :step, :linear, :spherical-linear, :cubic-spline
   (interpolation :accessor interpolation :initarg interpolation
                  :initform :linear)))

(defclass quat-key ()
  ;; time stored in seconds if possible
  ((time :accessor key-time :initarg time) ;; double
   ;; array of 4 floats
   (value :accessor value :initarg value)
   ;; :step, :linear, :spherical-linear, :cubic-spline
   (interpolation :accessor interpolation :initarg interpolation
                  :initform :linear)))

(defclass mesh-key ()
  ;; time stored in seconds if possible
  ((time :accessor key-time :initarg time) ;; double
   ;; unsigned-int
   (value :accessor value :initarg value)))

(defclass mesh-morph-key ()
  ;; time stored in seconds if possible
  ((time :accessor key-time :initarg time) ;; double
   ;; vector of unsigned-ints
   (values :accessor key-values :initarg values)
   ;; vector of doubles
   (weights :accessor weights :initarg weights)))

(defclass node-animation ()
  ;; todo: replace/supplement this with actual node
  ((node-name :accessor node-name :initarg node-name)
   ;; optional array of vector-key
   (position-keys :accessor position-keys :initarg position-keys)
   (scaling-keys :accessor scaling-keys :initarg scaling-keys)
   ;; optional array of quat-key
   (rotation-keys :accessor rotation-keys :initarg rotation-keys)
   (pre-state :accessor pre-state :initarg pre-state)
   (post-state :accessor post-state :initarg post-state)))



;;;;;;;;; todo: split out translator stuff into separate file, so this
;;;;;;;;; can be mostly API level stuff (classes, exported functions, etc)

(defmacro translate-ai-array (translator count source
                              &key (type :pointer) (indirect t))
  ;; fixme: possibly should add option to treat null pointers as error?
  `(progn
     ,(alexandria:once-only (count source)
        `(when (not (cffi:null-pointer-p ,source))
           (make-array
            ,count
            :initial-contents
            (loop for i below ,count
                  collect
                  ,(if indirect
                       `(,translator (cffi:mem-aref ,source '(:pointer ,type) i))
                       `(,translator (cffi:mem-aptr ,source ',type i)))))))))

;;todo: with-imported-scene ?

(defun decode-string (pointer length)
  ;; fixme: what is correct way to do this in cffi?
  (if (plusp length)
      (or (ignore-errors
           (cffi:foreign-string-to-lisp pointer :encoding :utf-8
                                                :count length))
          (cffi:foreign-string-to-lisp pointer :encoding :latin1
                                               :count length))
      ""))

(defun translate-ai-string (str)
  (when (null-pointer-p str)
    (return-from translate-ai-string nil))
  (with-foreign-slots* ((%ai:length (:pointer %ai:data)) str
                        (:struct %ai:ai-string))
    (decode-string %ai:data %ai:length)))

;; material properties use u32 for length instead of size_t
(defun translate-ai-string32 (str)
  (with-foreign-slots* ((%ai:length (:pointer %ai:data)) str
                        (:struct %ai::ai-string32))
    (decode-string %ai:data %ai:length)))

(defun translate-ai-matrix-4x4 (m)
  (when (not (cffi:null-pointer-p m))
    (make-array 16 :element-type 'single-float
                   :initial-contents
                   (loop for i below 16
                         collect (cffi:mem-aref m '%ai:ai-real i)))))

(defun translate-ai-metadata-entry (m) ;; 3.1.0+
  (with-foreign-slots* (((:pointer %ai:m-type)
                         %ai:data)
                        m
                        (:struct %ai::ai-metadata-entry))
    ;; bypass enum translation on the struct definition
    (unless (cffi:null-pointer-p %ai:data)
      (let ((type (cffi:mem-ref %ai:m-type :int)))
        (case type
          (0 (not (zerop (cffi:mem-ref %ai:data :int)))) ;; bool
          (1 (cffi:mem-ref %ai:data :int))
          (2 (cffi:mem-ref %ai:data :uint64))
          (3 (cffi:mem-ref %ai:data :float))
          (4 (cffi:mem-ref %ai:data :double))
          (5 (translate-ai-string %ai:data))
          (6 (translate-ai-vector3d %ai:data))
          ;; these enums were added in later versions, not sure if they
          ;; should actually be restricted though? should probably
          ;; return (list :unsupported type) if so.
          (7 (progn ;; %ai:v>= (5 1 0)
               (translate-ai-metadata %ai:data)))
          (8 (progn ;; %ai:v>= (5 3 0)
               (cffi:mem-ref %ai:data :int64)))
          (9 (progn ;; %ai:v>= (5 3 0)
               (cffi:mem-ref %ai:data :uint32)))
          (t (list :unknown-metadata-type type)))))))

(defun translate-ai-metadata (m)
  (%ai:v>= (3 1 0)
    (unless (null-pointer-p m)
      (with-foreign-slots* ((%ai:m-num-properties
                             %ai::m-keys
                             %ai::m-values)
                            m
                            (:struct %ai::ai-metadata))
        (let* ((n %ai:m-num-properties)
               (k (translate-ai-array translate-ai-string
                                      n
                                      %ai::m-keys
                                      :type (:struct %ai:ai-string)
                                      :indirect nil))
               (v (translate-ai-array translate-ai-metadata-entry
                                      %ai:m-num-properties
                                      %ai::m-values
                                      :type (:struct %ai::ai-metadata-entry)
                                      :indirect nil))
               (h (make-hash-table)))
          (when (and k v)
            (loop for i below n
                  do (setf (gethash (aref k i) h)
                           (aref v i))))
          h)))))

(defun translate-ai-node (node)
  ;; (print (cffi:foreign-slot-value node '(:struct %ai:ai-node) '%ai:m-num-meshes))
  (when (null-pointer-p node)
    (return-from translate-ai-node nil))
  (with-foreign-slots* (((:pointer %ai:m-name)
                         (:pointer %ai:m-transformation) %ai:m-parent
                         %ai:m-num-children %ai:m-children
                         %ai:m-num-meshes %ai:m-meshes
                         %ai::m-metadata)
                        node (:struct %ai:ai-node))
    (when *translate-verbose*
      (format t "load node ~s, ~s children ~s meshes~%"
              (translate-ai-string %ai:m-name)
              %ai:m-num-children %ai:m-num-meshes))
    (let ((new
            (make-instance
             'node
             'name (translate-ai-string %ai:m-name)
             'transform (translate-ai-matrix-4x4 %ai:m-transformation)
             ;; we replace this later, since we might not have created parent yet
             'parent (unless (cffi:null-pointer-p %ai:m-parent) %ai:m-parent)
             'children (remove
                        nil
                        (make-array
                         %ai:m-num-children
                         :initial-contents
                         (loop for i below %ai:m-num-children
                               collect (translate-ai-node
                                        (cffi:mem-aref %ai:m-children :pointer i)))))
             'meshes (make-array %ai:m-num-meshes
                                 :element-type '(unsigned-byte 32)
                                 :initial-contents
                                 (loop for i below %ai:m-num-meshes
                                       collect (cffi:mem-aref %ai:m-meshes
                                                              :unsigned-int i)))
             'metadata (%ai:v>= (3 1 0)
                         (translate-ai-metadata %ai::m-metadata)))))
      (loop for c across (children new)
            when (cffi:pointer-eq node (parent c))
              do (setf (parent c) new)
            else do (error "parent of child not = current node? ~s /= ~s"
                           node (parent c)))
      new)))

(defmacro translate-vector (pointer count type cl-type)
  (alexandria:with-gensyms (i)
    `(make-array ,count
                 :element-type ',cl-type
                 :initial-contents
                 (loop for ,i below ,count
                       collect (cffi:mem-aref ,pointer ',type ,i)))))

;; possibly should just combine these into a generic 'struct/array with
;; N-floats -> array' function?
(defun translate-ai-vector2 (v)
  (when (not (cffi:null-pointer-p v))
    ;;(format t "translating vector3d ~s~%" v)
    (translate-vector v 2 %ai:ai-real %ai:real)))

(defun translate-ai-vector3d (v)
  (when (not (cffi:null-pointer-p v))
    ;;(format t "translating vector3d ~s~%" v)
    (translate-vector v 3 %ai:ai-real %ai:real)))

(defun translate-ai-vector4 (v)
  (when (not (cffi:null-pointer-p v))
    (translate-vector v 4 %ai:ai-real %ai:real)))

(defun translate-ai-color3f (c)
  (when (not (cffi:null-pointer-p c))
    (%ai::vcase
      ((5 4 3) (translate-vector c 3 :float 'single-float))
      ((4 0 0) (translate-vector c 3 %ai:ai-real %ai:real))
      ((3 0 0) (translate-vector c 3 :float 'single-float)))))

(defun translate-ai-color4f (c)
  (when (not (cffi:null-pointer-p c))
    (%ai::vcase
      ((5 4 3) (translate-vector c 4 :float 'single-float))
      ((4 0 0) (translate-vector c 4 %ai:ai-real %ai:real))
      ((3 0 0) (translate-vector c 4 :float 'single-float)))))

(defun translate-uint (p)
  (cffi:mem-aref p :unsigned-int))

(defun translate-ai-face (f)
  (with-foreign-slots* ((%ai:m-num-indices %ai:m-indices) f
                        (:struct %ai:ai-face))
    (translate-ai-array translate-uint %ai:m-num-indices %ai:m-indices
                        :type :unsigned-int :indirect nil)))

(defun sequence-right-trim (bag sequence)
  (let ((pos (position-if-not #'(lambda (x) (member x bag)) sequence :from-end t)))
    (subseq sequence 0 (if pos (1+ pos) 0))))

(defun translate-ai-vertex-weight (w)
  (with-foreign-slots* ((%ai:m-vertex-id %ai:m-weight) w
                        (:struct %ai:ai-vertex-weight))
    (make-instance 'vertex-weight 'id %ai:m-vertex-id 'weight %ai:m-weight)))

(defun translate-ai-bone (b)
  (with-foreign-slots* (((:pointer %ai:m-name) %ai:m-num-weights
                         %ai:m-weights (:pointer %ai:m-offset-matrix))
                        b (:struct %ai:ai-bone))
    (when *translate-verbose*
      (format t "load bone ~s, ~s weights~%"
              (translate-ai-string %ai:m-name)
              %ai:m-num-weights))
    (make-instance
     'bone
     'name (translate-ai-string %ai:m-name)
     'weights (translate-ai-array translate-ai-vertex-weight
                                  %ai:m-num-weights %ai:m-weights
                                  :type (:struct %ai:ai-vertex-weight) :indirect nil)
     'offset-matrix (translate-ai-matrix-4x4 %ai:m-offset-matrix))))

(defun translate-ai-anim-mesh (m)
  (format t "translate anim mesh #x~x~%" m)
  (%ai:v>= (3 0 0)
    (with-foreign-slots* (((:pointer %ai:m-name)
                           %ai:m-vertices
                           %ai:m-normals
                           %ai:m-tangents
                           %ai:m-bitangents
                           %ai:m-colors
                           %ai:m-texture-coords
                           %ai:m-num-vertices
                           %ai:m-weight)
                          m (:struct %ai::ai-anim-mesh))
      (make-instance
       'anim-mesh
       'name (translate-ai-string %ai:m-name)
       'vertices (translate-ai-array translate-ai-vector3d %ai:m-num-vertices
                                     %ai:m-vertices
                                     :type (:struct %ai:ai-vector-3d)
                                     :indirect nil)
       'normals (translate-ai-array translate-ai-vector3d %ai:m-num-vertices
                                    %ai:m-normals
                                    :type (:struct %ai:ai-vector-3d)
                                    :indirect nil)
       'tangents (translate-ai-array translate-ai-vector3d %ai:m-num-vertices
                                     %ai:m-tangents
                                     :type (:struct %ai:ai-vector-3d)
                                     :indirect nil)
       'bitangents (translate-ai-array translate-ai-vector3d %ai:m-num-vertices
                                       %ai:m-bitangents
                                       :type (:struct %ai:ai-vector-3d)
                                       :indirect nil)
       'colors (coerce
                (sequence-right-trim
                 '(nil)
                 (loop for i below %ai::+ai-max-number-of-color-sets+
                       for c = (cffi:mem-aref %ai:m-colors :pointer i)
                       collect (translate-ai-array translate-ai-color4f
                                                   %ai:m-num-vertices
                                                   c
                                                   :type (:struct
                                                          %ai:ai-color-4d)
                                                   :indirect nil)))
                'vector)
       'texture-coords (coerce
                        (sequence-right-trim
                         '(nil)
                         (loop for i below %ai::+ai-max-number-of-texturecoords+
                               for tc = (cffi:mem-aref %ai:m-texture-coords
                                                       :pointer i)
                               collect (translate-ai-array translate-ai-vector3d
                                                           %ai:m-num-vertices
                                                           tc
                                                           :type (:struct %ai:ai-vector-3d)
                                                           :indirect nil)))
                        'vector)
       'weight %ai:m-weight))))

(defun translate-ai-mesh (m)
  (with-foreign-slots* ((%ai:m-primitive-types
                         %ai:m-num-vertices
                         %ai:m-num-faces
                         %ai:m-vertices
                         %ai:m-normals
                         %ai:m-tangents
                         %ai:m-bitangents
                         %ai:m-colors
                         %ai:m-texture-coords
                         %ai:m-num-uv-components
                         %ai:m-faces
                         %ai:m-num-bones
                         %ai:m-bones
                         %ai:m-material-index
                         (:pointer %ai:m-name)
                         %ai::m-num-anim-meshes
                         %ai::m-anim-meshes
                         %ai::m-method
                         (:pointer %ai::m-aabb)
                         %ai::m-texture-coords-names)
                        m (:struct %ai:ai-mesh))
    (when *translate-verbose*
      (format t "load mesh~%"))
    (make-instance
     'mesh
     'primitive-types %ai:m-primitive-types
     'vertices (translate-ai-array translate-ai-vector3d %ai:m-num-vertices
                                   %ai:m-vertices
                                   :type (:struct %ai:ai-vector-3d) :indirect nil)
     'normals (translate-ai-array translate-ai-vector3d %ai:m-num-vertices
                                  %ai:m-normals
                                  :type (:struct %ai:ai-vector-3d) :indirect nil)
     'tangents (translate-ai-array translate-ai-vector3d %ai:m-num-vertices
                                   %ai:m-tangents
                                   :type (:struct %ai:ai-vector-3d) :indirect nil)
     'bitangents (translate-ai-array translate-ai-vector3d %ai:m-num-vertices
                                     %ai:m-bitangents
                                     :type (:struct %ai:ai-vector-3d) :indirect nil)
     'colors (apply 'vector
                    (sequence-right-trim
                     '(nil)
                     (loop for i below %ai::+ai-max-number-of-color-sets+
                           for c = (cffi:mem-aref %ai:m-colors :pointer i)
                           collect (translate-ai-array translate-ai-color4f
                                                       %ai:m-num-vertices
                                                       c
                                                       :type (:struct  %ai:ai-color-4d)
                                                       :indirect nil))))
     'texture-coords
     (apply 'vector
            (sequence-right-trim
             '(nil)
             (loop for i below %ai::+ai-max-number-of-texturecoords+
                   for tc = (cffi:mem-aref %ai:m-texture-coords :pointer i)
                   collect (translate-ai-array translate-ai-vector3d
                                               %ai:m-num-vertices
                                               tc
                                               :type (:struct %ai:ai-vector-3d)
                                               :indirect nil))))
     'components-per-texture-coord
     (translate-ai-array translate-uint %ai::+ai-max-number-of-texturecoords+
                         %ai:m-num-uv-components
                         :type :unsigned-int :indirect nil)
     'faces (translate-ai-array translate-ai-face %ai:m-num-faces
                                %ai:m-faces
                                :type (:struct %ai:ai-face) :indirect nil)
     'bones (translate-ai-array translate-ai-bone %ai:m-num-bones
                                %ai:m-bones)
     'material-index %ai:m-material-index

     'name (%ai::v>= (3 0 0)
             (translate-ai-string %ai:m-name))
     'anim-meshes (%ai::v>= (3 0 0)
                    (translate-ai-array translate-ai-anim-mesh
                                        %ai::m-num-anim-meshes
                                        %ai::m-anim-meshes
                                        :type (:struct %ai::ai-anim-mesh)
                                        :indirect t))
     'morphing-method (%ai::v>= (4 0 0)
                        (cffi:foreign-enum-keyword '%ai::ai-morphing-method
                                                   %ai::m-method))
     'aabb (%ai::v>= (5 0 0)
             (translate-ai-aabb %ai::m-aabb))
     'texture-coord-names (%ai::v>= (5 1 0)
                            (sequence-right-trim
                             '(nil)
                             (translate-ai-array translate-ai-string
                                                 %ai::+ai-max-number-of-texturecoords+
                                                 %ai::m-texture-coords-names
                                                 :type (:struct %ai:ai-string)
                                                 :indirect t))))))

(defparameter *translate-anim-node-ticks-per-second* 0.0)

(defun translate-ai-vector-key (k)
  (with-foreign-slots* ((%ai:m-time (:pointer %ai:m-value)) k
                        (:struct %ai:ai-vector-key))
    (make-instance 'vector-key
                   'time (if (or (not *loader-translate-times*)
                                 (zerop *translate-anim-node-ticks-per-second*))
                             %ai:m-time
                             (/ %ai:m-time
                                *translate-anim-node-ticks-per-second*))
                   'value (translate-ai-vector3d %ai:m-value)
                   'interpolation (%ai:v>= (5 4 3) %ai:m-interpolation))))

(defun translate-ai-quaternion-key (k)
  (with-foreign-slots* ((%ai:m-time (:pointer %ai:m-value)) k
                        (:struct %ai:ai-quat-key))
    (make-instance 'quat-key
                   'time (if (or (not *loader-translate-times*)
                                 (zerop *translate-anim-node-ticks-per-second*))
                             %ai:m-time
                             (/ %ai:m-time
                                *translate-anim-node-ticks-per-second*))
                   'value (translate-ai-vector4 %ai:m-value)
                   'interpolation (%ai:v>= (5 4 3) %ai:m-interpolation))))

(defun trannslate-mesh-key (k)
  (with-foreign-slots* ((%ai:m-time %ai:m-value) k
                        (:struct %ai::ai-mesh-key))
    (make-instance 'mesh-key
                   'time (if (or (not *loader-translate-times*)
                                 (zerop *translate-anim-node-ticks-per-second*))
                             %ai:m-time
                             (/ %ai:m-time
                                *translate-anim-node-ticks-per-second*))
                   'value %ai:m-value)))

(defun trannslate-mesh-morph-key (k)
  (with-foreign-slots* ((%ai:m-time
                         %ai::m-values
                         %ai:m-weights
                         %ai::m-num-values-and-weights)
                        k
                        (:struct %ai::ai-mesh-morph-key))
    (make-instance 'mesh-morph-key
                   'time (if (or (not *loader-translate-times*)
                                 (zerop *translate-anim-node-ticks-per-second*))
                             %ai:m-time
                             (/ %ai:m-time
                                *translate-anim-node-ticks-per-second*))
                   'values (translate-vector %ai::m-values
                                             %ai::m-num-values-and-weights
                                             :unsigned-int
                                             (unsigned-byte 32))
                   'weights (translate-vector %ai:m-weights
                                              %ai::m-num-values-and-weights
                                              :double
                                              double-float))))

(defun translate-ai-anim-node (a)
  (with-foreign-slots* (((:pointer %ai:m-node-name)
                         %ai:m-num-position-keys
                         %ai:m-position-keys
                         %ai:m-num-rotation-keys
                         %ai:m-rotation-keys
                         %ai:m-num-scaling-keys
                         %ai:m-scaling-keys
                         %ai:m-pre-state
                         %ai:m-post-state)
                        a
                        (:struct %ai:ai-node-anim))
    (when *translate-verbose*
      (format t "load anim keys for node ~s, ~s / ~s / ~s keys~%"
              (translate-ai-string %ai:m-node-name)
              %ai:m-num-position-keys %ai:m-num-rotation-keys %ai:m-num-scaling-keys))
    (make-instance
     'node-animation
     'node-name (translate-ai-string %ai:m-node-name)
     'position-keys (translate-ai-array translate-ai-vector-key
                                        %ai:m-num-position-keys
                                        %ai:m-position-keys
                                        :type (:struct %ai:ai-vector-key) :indirect nil)
     'rotation-keys (translate-ai-array translate-ai-quaternion-key
                                        %ai:m-num-rotation-keys
                                        %ai:m-rotation-keys
                                        :type (:struct %ai:ai-quat-key) :indirect nil)
     'scaling-keys (translate-ai-array translate-ai-vector-key
                                       %ai:m-num-scaling-keys
                                       %ai:m-scaling-keys
                                       :type (:struct %ai:ai-vector-key) :indirect nil)
     'pre-state %ai:m-pre-state
     'post-state %ai:m-post-state)))

(defun translate-ai-mesh-anim (m)
  (with-foreign-slots* (((:pointer %ai:m-node-name)
                         %ai::m-num-keys
                         %ai::m-keys)
                        m (:struct %ai::ai-mesh-morph-anim))
    (list :name (translate-ai-string %ai:m-node-name)
          :keys (translate-ai-array trannslate-mesh-key
                                    %ai::m-num-keys
                                    %ai::m-keys
                                    :type (:struct %ai::ai-mesh-key)
                                    :indirect nil))))

(defun translate-ai-mesh-morph-anim (m)
  (with-foreign-slots* (((:pointer %ai:m-node-name)
                         %ai::m-num-keys
                         %ai::m-keys)
                        m (:struct %ai::ai-mesh-morph-anim))
    (list :name (translate-ai-string %ai:m-node-name)
          :keys (translate-ai-array trannslate-mesh-morph-key
                                    %ai::m-num-keys
                                    %ai::m-keys
                                    :type (:struct %ai::ai-mesh-morph-key)
                                    :indirect nil))))

(defun translate-ai-animation (a)
  (with-foreign-slots* (((:pointer %ai:m-name) %ai:m-duration
                         %ai:m-ticks-per-second %ai:m-num-channels
                         %ai:m-channels
                         %ai::m-num-mesh-channels
                         %ai::m-mesh-channels
                         %ai::m-num-mesh-morph-channels
                         %ai::m-mesh-morph-channels)
                        a
                        (:struct %ai:ai-animation))
    (when *translate-verbose*
      (format t "load animation ~s, ~s channels~%"
              (translate-ai-string %ai:m-name)
              %ai:m-num-channels))
    (let* ((*translate-anim-node-ticks-per-second*
             (if (zerop %ai:m-ticks-per-second)
                 *loader-default-animation-ticks-per-second*
                 %ai:m-ticks-per-second))
           (channels (translate-ai-array translate-ai-anim-node
                                         %ai:m-num-channels
                                         %ai:m-channels))
           (index (make-hash-table :test 'equal)))
      (when channels
        (loop for c across channels
              do (setf (gethash (node-name c) index) c)))
      (make-instance
       'animation
       'name (translate-ai-string %ai:m-name)
       'duration (if *loader-translate-times*
                     (if (zerop %ai:m-ticks-per-second)
                         (/ %ai:m-duration *loader-default-animation-ticks-per-second*)
                         (/ %ai:m-duration %ai:m-ticks-per-second))
                     %ai:m-duration)
       'ticks-per-second %ai:m-ticks-per-second
       'channels channels
       'index index
       'mesh-channels (%ai:v>= (3 1 0)
                        (translate-ai-array
                         translate-ai-mesh-anim
                         %ai::m-num-mesh-channels
                         %ai::m-mesh-channels
                         :type (:struct %ai::ai-mesh-anim)
                         :indirect t))
       'mesh-morph-channels (%ai:v>= (4 0 0)
                              (translate-ai-array
                               translate-ai-mesh-morph-anim
                               %ai::m-num-mesh-morph-channels
                               %ai::m-mesh-morph-channels
                               :type (:struct %ai::ai-mesh-morph-anim)
                               :indirect t))))))

(defun translate-generic-material-property (key p)
  (with-foreign-slots* ((%ai:m-semantic
                         %ai:m-index
                         %ai:m-data-length
                         %ai:m-type
                         %ai:m-data)
                        p (:struct %ai:ai-material-property))
    (flet ((data-array (lisp-type cffi-type size)
             (when *translate-verbose*
               (unless (zerop (mod %ai:m-data-length size))
                 (break "generic material property not multiple of expected size?~% size = ~s, expected multiple of ~s"
                        %ai:m-data-length size)))
             (if (= %ai:m-data-length size)
                 (cffi:mem-aref %ai:m-data cffi-type)
                 (make-array (/ %ai:m-data-length size)
                             :element-type lisp-type
                             :initial-contents
                             (loop for i below (/ %ai:m-data-length size)
                                   collect (cffi:mem-aref %ai:m-data
                                                          cffi-type i))))))
      (let ((data (ecase %ai:m-type
                    (:ai-pti-float (data-array 'single-float :float 4))
                    (:ai-pti-double (data-array 'double-float :double 8)) ;; 3.3+
                    (:ai-pti-string (translate-ai-string32 %ai:m-data))
                    (:ai-pti-integer (data-array '(signed-byte 32) :int 4))
                    (:ai-pti-buffer (data-array '(unsigned-byte 8)
                                                :unsigned-char 1)))))
        (when *translate-verbose*
          (if (eq %ai:m-semantic :ai-texture-type-none)
              (format t "material property: ~s = ~s~%" key data)
              (format t "material property: ~s / ~s, ~s == ~s~%"
                      key %ai:m-semantic %ai:m-index data)))
        (list key %ai:m-semantic %ai:m-index data)))))

(defun translate-ai-uv-transform (x)
  (with-foreign-slots* (((:pointer %ai:m-translation) (:pointer %ai:m-scaling)
                         %ai:m-rotation) x
                        (:struct %ai:ai-uv-transform))
    ;; rotation is radians, ccw around 0.5,0.5 in UV space, default 0
    (list (translate-ai-vector2 %ai:m-translation)
          (translate-ai-vector2 %ai:m-scaling)
          %ai:m-rotation)))

(defun translate-ai-material-property (p)
  (with-foreign-slots* (((:pointer %ai:m-key)
                         %ai:m-semantic
                         %ai:m-index
                         %ai:m-data-length
                         %ai:m-type
                         %ai:m-data)
                        p (:struct %ai:ai-material-property))
    (let ((key (translate-ai-string %ai:m-key)))
      (labels ((k= (s) (string= key s))
               (single-value (type)
                 ;; work around bug in ai gltf importer
                 ;; https://github.com/assimp/assimp/issues/2997
                 (when (and (member *wrapper-version*
                                    '(:|5.0| :|5.1| :|5.2| :|5.3| :|5.3.1|))
                            (= %ai:m-data-length 1)
                            (eql type :uint)
                            (eql %ai:m-type :ai-pti-buffer))
                   (return-from single-value
                     (cffi:mem-ref %ai:m-data :uint8)))
                 (assert (= %ai:m-data-length 4) ()
                         "wrong data size? key=~s~%expected 4 got ~s~% type=~s, m-type=~s~%" key
                         %ai:m-data-length type %ai:m-type)
                 (cffi:mem-aref %ai:m-data type))
               (keyword (name type)
                 (when (k= name)
                   (when *translate-verbose*
                     (format t "~s = ~s~%" name (single-value type)))
                   (list key %ai:m-semantic %ai:m-index (single-value type))))
               (flag (name)
                 (when (k= name)
                   (when *translate-verbose*
                     (format t "~s = ~s = ~s~%" key (single-value :uint)
                             (not (zerop (single-value :uint)))))
                   (list key %ai:m-semantic %ai:m-index
                         (not (zerop (single-value :uint)))))))
        (cond
          ((keyword "$mat.shadingm" '%ai:ai-shading-mode))
          ((keyword "$mat.blend" '%ai:ai-blend-mode))
          ((flag "$mat.twosided"))
          ((flag "$mat.wireframe"))
          ((keyword "$tex.op" '%ai:ai-texture-op))
          ((keyword "$tex.mapmodeu" '%ai:ai-texture-map-mode))
          ((keyword "$tex.mapmodev" '%ai:ai-texture-map-mode))
          ((keyword "$tex.mapping" '%ai:ai-texture-mapping))
          ((keyword "$tex.flags" '%ai:ai-texture-flags))
          #++((keyword "$tex.blend" ai-real))
          ((k= "$tex.uvtrafo")
           ;; some files have extra bytes here, not sure if there is
           ;; anything useful there or not...
           (assert (>= %ai:m-data-length
                       (cffi:foreign-type-size '(:struct %ai:ai-uv-transform)))
                   () "uv-trafo data length =~s expected ~s"
                   %ai:m-data-length
                   (cffi:foreign-type-size '(:struct %ai:ai-uv-transform)))
           (let ((x (translate-ai-uv-transform %ai:m-data)))
             (when *translate-verbose*
               (format t "~s = ~s~%" key x))
             (list key %ai:m-semantic %ai:m-index x)))
;;; "$tex.file" "$tex.uvwsrc" "$tex.mapaxis"

          (t (translate-generic-material-property key p)))))))

(defun translate-ai-material (m)
  (with-foreign-slots* ((%ai:m-num-properties %ai:m-properties)
                        m (:struct %ai:ai-material))
    (when *translate-verbose*
      (format t "loading material, ~s properties~%" %ai:m-num-properties))
    ;; keys are theoretically case insensitive, not sure if they ever
    ;; vary though, since there is a limited set or predefined values
    (loop with hash = (make-hash-table :test 'equalp)
          for i below %ai:m-num-properties
          for (key semantic index value) = (translate-ai-material-property
                                            (cffi:mem-aref %ai:m-properties
                                                           :pointer i))
          do (if (eq semantic :ai-texture-type-none)
                 (progn
                   (when (and *translate-verbose*
                              (gethash key hash))
                     (format t "duplicate key ~s in material? ~s / ~s~%"
                             key (gethash key hash) value))
                   (setf (gethash key hash) value))
                 (push (list semantic index value) (gethash key hash nil)))
          finally (progn (when *translate-verbose*
                           (format t " == ~s~%" (alexandria:hash-table-plist hash)))
                         (return hash)))))

(defun translate-ai-texture (tx)
  (with-foreign-slots* ((%ai:m-width
                         %ai:m-height
                         (:pointer %ai:ach-format-hint)
                         %ai:pc-data
                         (:pointer %ai::m-filename))
                        tx (:struct %ai:ai-texture))
    (let* ((ach-format-hint
             (cffi:foreign-string-to-lisp %ai:ach-format-hint
                                          :encoding :latin1
                                          :max-chars (%ai::vcase
                                                       ((3 0 0) 4)
                                                       ((4 0 0) 9)))))
      (when *translate-verbose*
        (format t "loaded embedded texture ~s x ~s~%" %ai:m-width %ai:m-height))
      (if (zerop %ai:m-height)
          (list :compressed-texture
                :width %ai:m-width :height %ai:m-height
                :format ach-format-hint
                :filename (%ai:v>= (5 0 0)
                            (translate-ai-string %ai::m-filename))
                :data
                (loop with a = (make-array %ai:m-width
                                           :element-type '(unsigned-byte 8))
                      for i below %ai:m-width
                      do (setf (aref a i)
                               (cffi:mem-aref %ai:pc-data :unsigned-char i))
                      finally (return a)))
          (list :uncompressed-texture
                :width %ai:m-width :height %ai:m-height
                :format ach-format-hint
                :filename (%ai:v>= (5 0 0)
                            (translate-ai-string %ai::m-filename))
                :data
                (loop with a = (make-array (* %ai:m-width %ai:m-height 4)
                                           :element-type '(unsigned-byte 8)
                                           :initial-element 255)
                      for i below (* %ai:m-width %ai:m-height 4)
                      do (setf (aref a i)
                               (cffi:mem-aref %ai:pc-data :unsigned-char i))
                      finally (return a)))))))

(defun translate-ai-light (l)
  (with-foreign-slots* (((:pointer %ai:m-name)
                         %ai:m-type
                         (:pointer %ai:m-position)
                         (:pointer %ai:m-direction)
                         %ai:m-up
                         %ai:m-attenuation-constant
                         %ai:m-attenuation-linear
                         %ai:m-attenuation-quadratic
                         (:pointer %ai:m-color-diffuse)
                         (:pointer %ai:m-color-specular)
                         (:pointer %ai:m-color-ambient)
                         %ai:m-angle-inner-cone
                         %ai:m-angle-outer-cone
                         %ai::m-size)
                        l (:struct %ai:ai-light))
    (when *translate-verbose* (format t "translating light = "))
    (ecase %ai:m-type
      (:ai-light-source-undefined
       (list %ai:m-type
             :name (translate-ai-string %ai:m-name)))
      (:ai-light-source-directional
       (list %ai:m-type
             :name (translate-ai-string %ai:m-name)
             :direction (translate-ai-vector3d %ai:m-direction)
             :diffuse (translate-ai-vector3d %ai:m-color-diffuse)
             :specular (translate-ai-vector3d %ai:m-color-specular)
             :ambient (translate-ai-vector3d %ai:m-color-ambient)
             :up (%ai:v>= (3 3 0) %ai::m-up)))

      (:ai-light-source-point
       (list %ai:m-type
             :name (translate-ai-string %ai:m-name)
             :position (translate-ai-vector3d %ai:m-position)
             :diffuse (translate-ai-vector3d %ai:m-color-diffuse)
             :specular (translate-ai-vector3d %ai:m-color-specular)
             :ambient (translate-ai-vector3d %ai:m-color-ambient)))

      (:ai-light-source-spot
       (list %ai:m-type
             :name (translate-ai-string %ai:m-name)
             :position (translate-ai-vector3d %ai:m-position)
             :direction (translate-ai-vector3d %ai:m-direction)
             :diffuse (translate-ai-vector3d %ai:m-color-diffuse)
             :specular (translate-ai-vector3d %ai:m-color-specular)
             :ambient (translate-ai-vector3d %ai:m-color-ambient)
             :inner-angle %ai:m-angle-inner-cone
             :outer-angle %ai:m-angle-outer-cone
             :up (%ai:v>= (3 3 0) %ai::m-up)))

      (:ai-light-source-ambient ;; 3.2+
       (list %ai:m-type
             :name (translate-ai-string %ai:m-name)
             :diffuse (translate-ai-vector3d %ai:m-color-diffuse)
             :specular (translate-ai-vector3d %ai:m-color-specular)
             :ambient (translate-ai-vector3d %ai:m-color-ambient)
             :up (%ai:v>= (3 3 0) %ai::m-up)))

      (:ai-light-source-area ;; 3.3+
       (list %ai:m-type
             :name (translate-ai-string %ai:m-name)
             :position (translate-ai-vector3d %ai:m-position)
             :direction (translate-ai-vector3d %ai:m-direction)
             :diffuse (translate-ai-vector3d %ai:m-color-diffuse)
             :specular (translate-ai-vector3d %ai:m-color-specular)
             :ambient (translate-ai-vector3d %ai:m-color-ambient)
             :area %ai:m-angle-inner-cone
             :up (%ai:v>= (3 3 0) %ai::m-up)
             :size (%ai:v>= (3 3 0) %ai::m-size))))))

(defun translate-ai-aabb (a)
  (with-foreign-slots* (((:pointer %ai::m-min)
                         (:pointer %ai::m-max))
                        a (:struct %ai::ai-aabb))
    (make-instance 'aabb
                   'mmin (translate-ai-vector3d %ai::m-min)
                   'mmax (translate-ai-vector3d %ai::m-max))))

(defun translate-ai-camera (c)
  (with-foreign-slots* (((:pointer %ai:m-name)
                         (:pointer %ai:m-position)
                         (:pointer %ai:m-up)
                         (:pointer %ai:m-look-at)
                         %ai:m-horizontal-fov
                         %ai:m-clip-plane-near
                         %ai:m-clip-plane-far
                         %ai:m-aspect
                         %ai::m-orthographic-width)
                        c (:struct %ai:ai-camera))
    (when *translate-verbose*
      (format t "translate camera ~s~%" (translate-ai-string %ai:m-name)))
    (list (translate-ai-string %ai:m-name)
          :position (translate-ai-vector3d %ai:m-position)
          :up (translate-ai-vector3d %ai:m-up)
          :look-at (translate-ai-vector3d %ai:m-look-at)
          :horizontal-fov %ai:m-horizontal-fov
          :clip-near %ai:m-clip-plane-near
          :clip-far %ai:m-clip-plane-far
          :aspect %ai:m-aspect
          :orthographic-width (%ai:v>= (5 1 0) %ai::m-orthographic-width))))

#++
(defun translate-ai-skeleton-bone (sb)
  (with-foreign-slots* ((%ai:m-parent
                         %ai:m-armature
                         %ai:m-node
                         %ai:m-num-weights
                         %ai:m-mesh-id
                         %ai:m-weights
                         (:pointer %ai:m-offset-matrix)
                         (:pointer %ai:m-local-matrix))
                        sb (:struct %ai:ai-skeleton-bone))
    (list
     :parent %ai:m-parent
     :weights (translate-ai-array translate-ai-vertex-weight
                                  %ai:m-num-weights %ai:m-weights)
     :offset-matrix (translate-ai-matrix-4x4 %ai:m-offset-matrix)
     :local-matrix (translate-ai-matrix-4x4 %ai:m-local-matrix)

     ;; todo: we probably need to build a map of pointers to instances
     ;; for these so we can have the same object in
     ;; multiple places?
     :armature (???-ai-node %ai:m-armature)
     :node (???-ai-node %ai:m-node)
     :mesh-id (???-ai-mesh %ai:m-mesh-id))))

#++
(defun translate-ai-skeleton (s)
  (with-foreign-slots* (((:pointer %ai:m-name)
                         %ai:m-num-bones
                         %ai:m-bones)
                        s (:struct %ai:ai-skeleton))
    (when *translate-verbose*
      (format t "translate skeleton ~s~%" (translate-ai-string %ai:m-name)))
    (list (translate-ai-string %ai:m-name)
          :bones (translate-ai-array translate-ai-skeleton-bone
                                     %ai:m-num-bones %ai:m-bones))))

(defun check-version ()
  (let* ((major (%ai:ai-get-version-major))
         (minor (%ai:ai-get-version-minor))
         (v5.3 (or (> major 5) (and (= major 5) (>= minor 3))))
         (patch (if v5.3 (%ai::ai-get-version-patch) 0))
         (version (intern (if v5.3
                              (format nil "~a.~a.~a" major minor patch)
                              (format nil "~a.~a" major minor))
                          :keyword)))
    (unless (eql version %ai::*version*)
      (cerror "try using it anyway"
              "classimp was compiled for assimp version ~a, current assimp version is ~a" %ai::*version* version))))

(defun translate-ai-scene (scene)
  (check-version)
  (with-foreign-slots* ((%ai:m-flags
                         %ai:m-root-node
                         %ai:m-num-meshes %ai:m-meshes
                         %ai:m-num-materials %ai:m-materials
                         %ai:m-num-animations %ai:m-animations
                         %ai:m-num-textures %ai:m-textures
                         %ai:m-num-lights %ai:m-lights
                         %ai:m-num-cameras %ai:m-cameras
                         %ai:m-metadata
                         (:pointer %ai:m-name)
                         ;; not in bindings yet
                         %ai:m-num-skeletons %ai:m-skeletons
                         )
                        scene (:struct %ai:ai-scene))
    (make-instance
     'scene
     'flags %ai:m-flags
     'root (translate-ai-node %ai:m-root-node)
     'meshes (translate-ai-array translate-ai-mesh
                                 %ai:m-num-meshes %ai:m-meshes)
     'materials (translate-ai-array translate-ai-material
                                    %ai:m-num-materials %ai:m-materials)
     'animations (translate-ai-array translate-ai-animation
                                     %ai:m-num-animations %ai:m-animations)
     'textures (translate-ai-array translate-ai-texture
                                   %ai:m-num-textures %ai:m-textures)
     'lights (translate-ai-array translate-ai-light
                                 %ai:m-num-lights %ai:m-lights)
     'cameras (translate-ai-array translate-ai-camera
                                  %ai:m-num-cameras %ai:m-cameras)
     'metadata (%ai:v>= (5 0 0) (translate-ai-metadata %ai::m-metadata))
     'name (%ai:v>= (5 1 0) (translate-ai-string %ai::m-name))
     #++ #++
     'skeletons (%ai:v>= (5 3 0)
                  (translate-ai-array translate-ai-skeleton
                                      %ai:m-num-skeletons %ai:m-skeletons)))))

(defun translate-ai-importer-desc (desc)
  (%ai:v>= (3 2 0)
    (unless (null-pointer-p desc)
      (with-foreign-slots* ((%ai:m-name
                             %ai:m-author
                             %ai:m-maintainer
                             %ai:m-comments
                             %ai:m-flags
                             %ai:m-min-major
                             %ai:m-min-minor
                             %ai:m-max-major
                             %ai:m-max-minor
                             %ai:m-file-extensions)
                            desc (:struct %ai:ai-importer-desc))
        (list :name %ai:m-name
              :author %ai:m-author
              :maintainer %ai:m-maintainer
              :comments %ai:m-comments
              :flags %ai:m-flags
              :min-major %ai:m-min-major
              :min-minor %ai:m-min-minor
              :max-major %ai:m-max-major
              :max-minor %ai:m-max-minor
              :file-extensions (split-sequence:split-sequence
                                #\space %ai:m-file-extensions
                                :remove-empty-subseqs t))))))

;; copied from cl-glut
(defmacro without-fp-traps (&body body)
  #+(and sbcl (or x86 x86-64))
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body)
  #-(and sbcl (or x86 x86-64))
  `(progn ,@body))

(defun import-into-lisp (filename &key processing-flags raw-times properties)
  ;; see config.lisp for PROPERTIES values/usage
  (check-version)
  (let ((raw-scene nil) (*loader-translate-times* (not raw-times)))
    (prog1
        (unwind-protect
             (let ((flags (cffi:foreign-bitfield-value
                           '%ai:ai-post-process-steps processing-flags)))
               (when *translate-verbose*
                 (format t "  ai-import-file ~s~%" filename))
               (setf raw-scene
                     (without-fp-traps
                       (if properties
                           (with-property-store (store :properties properties)
                             (%ai:ai-import-file-ex-with-properties
                              (namestring filename) flags
                              (cffi:null-pointer) store))
                           (%ai:ai-import-file (namestring filename) flags))))
               (when (and (cffi:null-pointer-p raw-scene)
                          *translate-verbose*)
                 (format t "  import failed: ~s~%" (%ai:ai-get-error-string)))
               (unless (cffi:null-pointer-p raw-scene)
                 (when *translate-verbose*
                   (format t "  translate-scene~%"))
                 (translate-ai-scene raw-scene)))
          (when raw-scene
            (when *translate-verbose*
              (format t "  ai-release-import ~s~%" filename))
            (%ai:ai-release-import raw-scene)))
      (when *translate-verbose*
        (format t "import-into-lisp ~s done~%" filename)))))

(defun import-into-lisp/memory (foreign-pointer size
                                &key processing-flags raw-times properties
                                  (extension ""))
  (check-version)
  (let ((raw-scene nil) (*loader-translate-times* (not raw-times)))
    (prog1
        (unwind-protect
             (let ((flags (cffi:foreign-bitfield-value
                           '%ai:ai-post-process-steps processing-flags)))
               (when *translate-verbose*
                 (format t "  ai-import-memory ~x :~s (~s) ~%"
                         foreign-pointer size extension))
               (cffi:with-foreign-string (hint (if (stringp extension)
                                                   extension
                                                   ""))
                 (setf raw-scene
                       (without-fp-traps
                         (let ((hint (cond
                                       ((stringp extension)
                                        hint)
                                       ((cffi:pointerp extension)
                                        extension)
                                       (t (cffi:null-pointer)))))
                           (format t "==(~s)" (cffi:foreign-string-to-lisp hint))
                           (if properties
                               (with-property-store (store :properties properties)
                                 (%ai:ai-import-file-from-memory-with-properties
                                  foreign-pointer size flags
                                  hint
                                  store))
                               (%ai:ai-import-file-from-memory
                                foreign-pointer size flags hint))))))
               (unless (cffi:null-pointer-p raw-scene)
                 (when *translate-verbose*
                   (format t "  translate-scene~%"))
                 (translate-ai-scene raw-scene)))
          (when raw-scene
            (when *translate-verbose*
              (format t "  ai-release-import ~x~%" foreign-pointer))
            (%ai:ai-release-import raw-scene)))
      (when *translate-verbose*
        (format t "import-into-lisp/memory ~x done~%" foreign-pointer)))))

(defun import-into-lisp/string (string
                                &key processing-flags raw-times properties
                                  (extension ""))
  (cffi:with-foreign-string ((s l) string)
    (import-into-lisp/memory s l :processing-flags processing-flags
                                 :raw-times raw-times
                                 :properties properties
                                 :extension extension)))

;; todo: function to filter unused nodes as suggested in
;; http://assimp.sourceforge.net/lib_html/data.html#bones


;; cffi doesn't handle returning structs by value, so implementing some
;; loggers in lisp for now...
;; fixme: generalize this...
(cffi:defcallback log-to-*standard-output* :void ((message :string)
                                                  (user :pointer))
  (declare (ignore user))
  (format *standard-output* "AI|~a" message))

(defmacro with-log-to-stdout ((&rest r) &body body)
  (declare (ignore r))
  ;; fixme: error checking...
  (let ((log (gensym)))
    `(cffi:with-foreign-object (,log '(:struct %ai:ai-log-stream))
       (setf (cffi:foreign-slot-value ,log '(:struct %ai:ai-log-stream)
                                      '%ai:callback)
             (cffi:callback log-to-*standard-output*))
       #-classimp-broken-logging
       (%ai:ai-attach-log-stream ,log)
       (unwind-protect
            (progn
              ,@body)
         #-classimp-broken-logging
         (%ai:ai-detach-log-stream ,log)))))

(defun get-extension-list ()
  (cffi:with-foreign-object (p '(:struct %ai:ai-string))
    (setf (foreign-slot-value p '(:struct %ai:ai-string) '%ai:length) 0)
    (%ai::ai-get-extension-list p)
    (loop for a in (split-sequence:split-sequence #\; (translate-ai-string p))
          when (= 2 (mismatch a "*."))
            collect (subseq a 2)
          else collect (list a (mismatch a "*.")))))
;; todo: more loggers, file etc...

(defun get-importer-description-list ()
  (%ai:v>= (3 2 0)
    (loop for i below (%ai:ai-get-import-format-count)
          collect (translate-ai-importer-desc
                   (%ai:ai-get-import-format-description i)))))

(defun get-importer-description (extension)
  (%ai:v>= (3 2 0)
    (translate-ai-importer-desc
     (%ai:ai-get-importer-description
      ;; ai expects "foo", but accept ".foo" too for convenience
      (string-left-trim "." extension)))))
