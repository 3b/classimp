(cl:in-package :%open-asset-import-library)

;; try to determine library version at compile/load time
(cl:eval-when (:compile-toplevel :load-toplevel :execute)

  (cffi:defcfun ("aiGetVersionMajor" ai-get-version-major) :unsigned-int)
  (cffi:defcfun ("aiGetVersionMinor" ai-get-version-minor) :unsigned-int)

  (cl:defvar *version* :unknown)
  (cl:defvar *compiled* ())
  (cl:defun get-version-keyword ()
    (cl:let* ((major (cl:ignore-errors (ai-get-version-major)))
              (minor (cl:ignore-errors (ai-get-version-minor)))
              (new-version (cl:intern
                            (cl:format () "~a.~a" major minor) :keyword)))
      (cl:values new-version major minor)))
  (cl:defun %v= (req)
    (cl:ecase req
      (:3.0-3.3 (cl:member *version* '(:3.0 :3.1 :3.2 :3.3))) ;; exact versions
      (:3.0-4.1 (cl:member *version* '(:3.0 :3.1 :3.2 :3.3 :4.0 :4.1))) ;; exact versions
      (:3.0+ (cl:member *version* '(:3.0 :3.1 :3.2 :3.3 :4.0 :4.1 :5.0)))
      (:3.1+ (cl:member *version* '(:3.1 :3.2 :3.3 :4.0 :4.1 :5.0)))
      (:3.2+ (cl:member *version* '(:3.2 :3.3 :4.0 :4.1 :5.0)))
      (:3.3+ (cl:member *version* '(:3.3 :4.0 :4.1 :5.0)))
      (:>3.3 (cl:error "fix this"))
      (:4.0+ (cl:member *version* '(:4.0 :4.1 :5.0)))
      (:4.1+ (cl:member *version* '(:4.1 :5.0)))
      (:5.0+ (cl:member *version* '(:5.0))))))

(cl:eval-when (:compile-toplevel)
  (cl:setf *compiled* cl:t)
  (cl:when (cffi:foreign-library-loaded-p 'assimp)
    ;; try to check version of C library
    (cl:multiple-value-bind (new-version major minor)
        (get-version-keyword)
      (cl:when (cl:and major minor)
        (cl:unless (cl:case major
                     (3 (cl:<= 0 minor 3))
                     (4 (cl:<= 0 minor 1))
                     (5 (cl:= minor 0))
                     (t nil))
          (cl:error "trying to link against unsupported version of assimp. 3.0-5.0.x supported, got version ~a.~a"
                    major minor))
        (cl:setf *version* new-version)))))

(cl:eval-when (:load-toplevel :execute)
  (cl:when (cffi:foreign-library-loaded-p 'assimp)
    (cl:multiple-value-bind (new-version major minor)
        (get-version-keyword)
      (cl:when (cl:and major minor)
        (cl:unless (cl:case major
                     (3 (cl:<= 0 minor 3))
                     (4 (cl:<= 0 minor 1))
                     (5 (cl:= minor 0))
                     (t nil))
          (cl:error "trying to link against unsupported version of assimp. 3.0-5.0.x supported, got version ~a.~a"
                    major minor))
        (cl:cond
          ((cl:and *compiled* (cl:eql new-version *version*))
           ;; version matches
           )
          ((cl:not *compiled*)
           ;; not compiled, use current version
           (cl:setf *version* new-version))
          (cl:t ;; version mismatch
           (cl:error "classimp was compiled with different version of assimp, was ~a, now ~a" *version* new-version)))))))


;; long is not always equal to size_t, long is 4 bytes on Windows 64
#+cffi-features:x86-64 (cffi::defctype size-t :unsigned-long-long)
#-cffi-features:x86-64 (cffi::defctype size-t :unsigned-long)


;; versioned version of cffi:defstruct
(cl:defmacro defcstruct/v (name-and-options cl:&body fields)
  `(cffi:defcstruct ,name-and-options
     ,@(cl:loop for f in fields
          for v? = (cl:when (cl:typep f '(cl:cons cl:keyword))
                     (cl:car f))
          when v?
            append (cl:loop for (v vf) on f by #'cl::cddr
                       when (%v= v)
                         return (cl:list vf))
          else unless v?
                 collect f)))

;; versioned version of cffi:defcfnum
(cl:defmacro defcenum/v (name-and-options cl:&body fields)
  (cl:print `(cffi:defcenum ,name-and-options
      ,@(cl:loop for f in fields
           for v? = (cl:when (cl:typep f '(cl:cons cl:keyword (cl:cons cl:cons)))
                      (cl:car f))
           when v?
             append (cl:loop for (v vf) on f by #'cl::cddr
                       when (%v= v)
                         return (cl:list vf))
           else unless v?
                  collect f))))

(defcstruct/v ai-string ;; 3.0+
  (:3.0-4.1 (length size-t)
   :5.0+ (length :uint32))
  (data :char :count 1024))

;; 3.0+, used in material string properties (same as ai-string in 5+
(cffi:defcstruct ai-string32
  (length :uint32)
  (data :char :count 1024))

(cffi:defcstruct ai-vector-3d ;; 3.0+
  (x :float)
  (y :float)
  (z :float))

;; 5.0+
(cffi:defcstruct ai-aabb
  (m-min (:struct ai-vector-3d))
  (m-max (:struct ai-vector-3d)))

(cffi:defcstruct ai-camera ;; 3.0+
  (m-name (:struct ai-string))
  (m-position (:struct ai-vector-3d))
  (m-up (:struct ai-vector-3d))
  (m-look-at (:struct ai-vector-3d))
  (m-horizontal-fov :float)
  (m-clip-plane-near :float)
  (m-clip-plane-far :float)
  (m-aspect :float))

(cffi:defcenum (ai-origin :int) ;; 3.0+
  (:ai-origin-set 0)
  (:ai-origin-cur 1)
  (:ai-origin-end 2))

(cffi:defcstruct ai-color-3d ;; 3.0+
  (r :float)
  (g :float)
  (b :float))

(cffi:defcenum (ai-property-type-info :int)
  (:ai-pti-float 1)
  (:ai-pti-double 2) ;; >3.3
  (:ai-pti-string 3)
  (:ai-pti-integer 4)
  (:ai-pti-buffer 5))

(cffi::defctype ai-bool :int) ;; 3.0+

(cffi:defcfun ("aiIsExtensionSupported" ai-is-extension-supported) ai-bool
  (sz-extension :string))

(cffi:defcstruct ai-vector-2d ;; 3.0+
  (x :float)
  (y :float))

(cffi:defcenum (ai-return :int) ;; 3.0+
  (:ai-return-success 0)
  (:ai-return-failure -1)
  (:ai-return-outofmemory -3))

(cffi:defcstruct ai-vector-key ;; 3.0+
  (m-time :double)
  (m-value (:struct ai-vector-3d)))

(cffi:defcstruct ai-quaternion ;; 3.0+
  (w :float)
  (x :float)
  (y :float)
  (z :float))

(cffi:defcstruct ai-quat-key ;; 3.0+
  (m-time :double)
  (m-value (:struct ai-quaternion)))

(cffi:defcstruct ai-mesh-key ;; 3.1+
  (m-time :double)
  (m-value :unsigned-int))

(cffi:defcstruct ai-mesh-morph-key ;; 4.0+
  (m-time :double)
  (m-values (:pointer :unsigned-int))
  (m-weights (:pointer :double))
  (m-num-values-and-weights :unsigned-int))

(cffi:defcenum (ai-anim-behaviour :unsigned-int) ;; 3.0+
  (:ai-anim-behaviour-default 0)
  (:ai-anim-behaviour-constant 1)
  (:ai-anim-behaviour-linear 2)
  (:ai-anim-behaviour-repeat 3))

(cffi:defcstruct ai-node-anim ;; 3.0+
  (m-node-name (:struct ai-string))
  (m-num-position-keys :unsigned-int)
  (m-position-keys (:pointer (:struct ai-vector-key)))
  (m-num-rotation-keys :unsigned-int)
  (m-rotation-keys (:pointer (:struct ai-quat-key)))
  (m-num-scaling-keys :unsigned-int)
  (m-scaling-keys (:pointer (:struct ai-vector-key)))
  (m-pre-state ai-anim-behaviour)
  (m-post-state ai-anim-behaviour))

(cffi:defcstruct ai-mesh-anim ;; 3.1+
  (m-node-name (:struct ai-string))
  (m-num-keys :unsigned-int)
  (m-keys (:pointer (:struct ai-mesh-key))))

(cffi:defcstruct ai-mesh-morph-anim ;; 4.0+
  (m-node-name (:struct ai-string))
  (m-num-keys :unsigned-int)
  (m-keys (:pointer (:struct ai-mesh-morph-key))))

(defcstruct/v ai-animation ;; 3.0+
  (m-name (:struct ai-string))
  (m-duration :double)
  (m-ticks-per-second :double)
  (m-num-channels :unsigned-int)
  (m-channels (:pointer (:pointer (:struct ai-node-anim))))
  (:3.1+ (m-num-mesh-channels :unsigned-int))
  (:3.1+ (m-mesh-channels (:pointer (:pointer (:struct ai-mesh-anim)))))
  (:4.0+ (m-num-mesh-morph-channels :unsigned-int))
  (:4.0+ (m-mesh-morph-channels (:pointer (:pointer (:struct ai-mesh-morph-anim))))))


(cffi:defcstruct ai-matrix-3x-3 ;; 3.0+
  (a-1 :float)
  (a-2 :float)
  (a-3 :float)
  (b-1 :float)
  (b-2 :float)
  (b-3 :float)
  (c-1 :float)
  (c-2 :float)
  (c-3 :float))

(cffi:defcfun ("aiCreateQuaternionFromMatrix" ai-create-quaternion-from-matrix) :void
  (quat (:pointer (:struct ai-quaternion)))
  (mat (:pointer (:struct ai-matrix-3x-3))))

(cffi:defcenum (ai-texture-op :int)
  (:ai-texture-op-multiply 0)
  (:ai-texture-op-add 1)
  (:ai-texture-op-subtract 2)
  (:ai-texture-op-divide 3)
  (:ai-texture-op-smooth-add 4)
  (:ai-texture-op-signed-add 5))

(cffi:defcenum (ai-texture-map-mode :int)
  (:ai-texture-map-mode-wrap 0)
  (:ai-texture-map-mode-clamp 1)
  (:ai-texture-map-mode-mirror 2)
  (:ai-texture-map-mode-decal 3))

(defcenum/v (ai-texture-type :int) ;; 3.0+
  (:ai-texture-type-none 0)
  (:ai-texture-type-diffuse 1)
  (:ai-texture-type-specular 2)
  (:ai-texture-type-ambient 3)
  (:ai-texture-type-emissive 4)
  (:ai-texture-type-height 5)
  (:ai-texture-type-normals 6)
  (:ai-texture-type-shininess 7)
  (:ai-texture-type-opacity 8)
  (:ai-texture-type-displacement 9)
  (:ai-texture-type-lightmap 10)
  (:ai-texture-type-reflection 11)
  ;; in 5.0+, 12-17 are "PBR" material, and unknown is 18
  (:3.0-4.1 (:ai-texture-type-unknown 12)
   :5.0+ (:ai-texture-type-base-color 12))
  (:5.0+ (:ai-texture-type-normal-camera 13))
  (:5.0+ (:ai-texture-type-emission-color 14))
  (:5.0+ (:ai-texture-type-metalness 15))
  (:5.0+ (:ai-texture-type-diffuse-roughness 16))
  (:5.0+ (:ai-texture-type-ambient-occlusion 17))
  (:5.0+ (:ai-texture-type-unknown 18)))

(cffi:defcstruct ai-material-property ;; 3.0+
  (m-key (:struct ai-string))
  (m-semantic ai-texture-type)
  (m-index :unsigned-int)
  (m-data-length :unsigned-int)
  (m-type ai-property-type-info)
  (m-data (:pointer :char)))

(cffi:defcfun ("aiSetImportPropertyFloat" ai-set-import-property-float) :void
  (store :pointer) ;; added in 3.0
  (sz-name :string)
  (value :float))

(cffi:defcstruct ai-material ;; 3.0+
  (m-properties (:pointer (:pointer (:struct ai-material-property))))
  (m-num-properties :unsigned-int)
  (m-num-allocated :unsigned-int))

#-old-assimp
(cffi:defcfun ("aiGetMaterialProperty" ai-get-material-property) ai-return
  (p-mat (:pointer (:struct ai-material)))
  (p-key :string)
  (type ai-texture-type)
  (index :unsigned-int)
  (p-prop-out (:pointer (:pointer (:struct ai-material-property)))))

(cffi:defcstruct ai-matrix-4x-4 ;; 3.0+
  (a-1 :float)
  (a-2 :float)
  (a-3 :float)
  (a-4 :float)
  (b-1 :float)
  (b-2 :float)
  (b-3 :float)
  (b-4 :float)
  (c-1 :float)
  (c-2 :float)
  (c-3 :float)
  (c-4 :float)
  (d-1 :float)
  (d-2 :float)
  (d-3 :float)
  (d-4 :float))

(cffi:defcenum (ai-metadata-type :int) ;; 3.1+
  (:bool 0)
  (:int 1)
  (:uint64 2)
  (:float 3)
  (:double 4)
  (:ai-string 5)
  (:ai-vector-3d 6))

(cffi:defcstruct ai-metadata-entry ;; 3.1+
  (m-type ai-metadata-type)
  (data (:pointer :void)))

(cffi:defcstruct ai-metadata ;; 3.1+
  (m-num-properties :unsigned-int)
  (m-keys (:pointer (:struct ai-string)))
  (m-values (:pointer (:struct ai-metadata-entry))))

(defcstruct/v ai-node ;; 3.0+
  (m-name (:struct ai-string))
  (m-transformation (:struct ai-matrix-4x-4))
  (m-parent (:pointer (:struct ai-node)))
  (m-num-children :unsigned-int)
  (m-children (:pointer (:pointer (:struct ai-node))))
  (m-num-meshes :unsigned-int)
  (m-meshes (:pointer :unsigned-int))
  (:3.1+ (m-metadata (:pointer (:struct ai-metadata))))) ;; 3.1+

(cffi:defcstruct ai-color-4d ;; 3.0+
  (r :float)
  (g :float)
  (b :float)
  (a :float))

(cffi:defcstruct ai-face ;; 3.0+
  (m-num-indices :unsigned-int)
  (m-indices (:pointer :unsigned-int)))

(cffi:defcstruct ai-vertex-weight ;; 3.0+
  (m-vertex-id :unsigned-int)
  (m-weight :float))

(cffi:defcstruct ai-bone ;; 3.0+
  (m-name (:struct ai-string))
  (m-num-weights :unsigned-int)
  (m-weights (:pointer (:struct ai-vertex-weight)))
  (m-offset-matrix (:struct ai-matrix-4x-4)))

;; fixme: probably should grovel these, aiMesh.h says they shouldn't change
;; though, so ignoring for now...
;; ... they changed anyway :/
(cl:eval-when (:compile-toplevel :load-toplevel :execute) ;; 3.0+
  (cl:defconstant +ai-max-number-of-color-sets+ 8)
  (cl:defconstant +ai-max-number-of-texturecoords+ 8))

;; unused in 3.0-3.3?
(cffi:defcstruct ai-anim-mesh
  (m-name (:struct ai-string))
  (m-vertices (:pointer (:struct ai-vector-3d)))
  (m-normals (:pointer (:struct ai-vector-3d)))
  (m-tangents (:pointer (:struct ai-vector-3d)))
  (m-bitangents (:pointer (:struct ai-vector-3d)))
  (m-colors (:pointer (:struct ai-color-4d))
            :count #.+ai-max-number-of-color-sets+)
  (m-texture-coords (:pointer (:struct ai-vector-3d))
                    :count #.+ai-max-number-of-texturecoords+)
  (m-num-vertices :unsigned-int)
  (m-weight :float))

(cffi:defcenum (ai-morphing-method :int)
  (:vertex-blend 1)
  (:morph-normalized 2)
  (:morph-relative 3))

(cffi:defcstruct ai-mesh ;; 3.0+
  (m-primitive-types :unsigned-int)
  (m-num-vertices :unsigned-int)
  (m-num-faces :unsigned-int)
  (m-vertices (:pointer (:struct ai-vector-3d)))
  (m-normals (:pointer (:struct ai-vector-3d)))
  (m-tangents (:pointer (:struct ai-vector-3d)))
  (m-bitangents (:pointer (:struct ai-vector-3d)))
  (m-colors (:pointer (:struct ai-color-4d))
            :count #.+ai-max-number-of-color-sets+)
  (m-texture-coords (:pointer (:struct ai-vector-3d))
                    :count #.+ai-max-number-of-texturecoords+)
  (m-num-uv-components :unsigned-int
                       :count #.+ai-max-number-of-texturecoords+)
  (m-faces (:pointer (:struct ai-face)))
  (m-num-bones :unsigned-int)
  (m-bones (:pointer (:pointer (:struct ai-bone))))
  (m-material-index :unsigned-int)
  (m-name (:struct ai-string)) ;; 3.0+
  (m-num-anim-meshes :unsigned-int) ;; 3.0+, unused up to 3.3
  (m-anim-meshes (:pointer (:pointer (:struct ai-anim-mesh)))) ;; 3.0+, unused up to 3.3
  (m-method :unsigned-int) ;; >3.3
  (m-aabb (:struct ai-aabb)))

(cffi:defcstruct ai-texel ;; 3.0+
  (b :unsigned-char)
  (g :unsigned-char)
  (r :unsigned-char)
  (a :unsigned-char))

(defcstruct/v ai-texture ;; 3.0+
  (m-width :unsigned-int)
  (m-height :unsigned-int)
  ;; ach-format-hint changed size in 4.0+
  (:3.0-3.3 (ach-format-hint :char :count 4)
   :4.0+ (ach-format-hint :char :count 9))
  (pc-data (:pointer (:struct ai-texel)))
  (:5.0+ (m-filename (:pointer (:struct ai-string)))))

(cffi:defcenum (ai-light-source-type :int) ;; 3.0+
  (:ai-light-source-undefined 0)
  (:ai-light-source-directional 1)
  (:ai-light-source-point 2)
  (:ai-light-source-spot 3)
  (:ai-light-source-ambient 4) ;; 3.2+
  (:ai-light-source-area 5)) ;; 3.3+

(defcstruct/v ai-light ;; 3.0+
  (m-name (:struct ai-string))
  (m-type ai-light-source-type)
  (m-position (:struct ai-vector-3d))
  (m-direction (:struct ai-vector-3d))
  (:3.3+ (m-up (:struct ai-vector-3d))) ;; 3.3
  (m-attenuation-constant :float)
  (m-attenuation-linear :float)
  (m-attenuation-quadratic :float)
  (m-color-diffuse (:struct ai-color-3d))
  (m-color-specular (:struct ai-color-3d))
  (m-color-ambient (:struct ai-color-3d))
  (m-angle-inner-cone :float)
  (m-angle-outer-cone :float)
  (:3.3+ (m-size (:struct ai-vector-2d))))

(defcstruct/v ai-scene ;; 3.0+
  (m-flags :unsigned-int)
  (m-root-node (:pointer (:struct ai-node)))
  (m-num-meshes :unsigned-int)
  (m-meshes (:pointer (:pointer (:struct ai-mesh))))
  (m-num-materials :unsigned-int)
  (m-materials (:pointer (:pointer (:struct ai-material))))
  (m-num-animations :unsigned-int)
  (m-animations (:pointer (:pointer (:struct ai-animation))))
  (m-num-textures :unsigned-int)
  (m-textures (:pointer (:pointer (:struct ai-texture))))
  (m-num-lights :unsigned-int)
  (m-lights (:pointer (:pointer (:struct ai-light))))
  (m-num-cameras :unsigned-int)
  (m-cameras (:pointer (:pointer (:struct ai-camera))))
  (:3.0-4.1 (m-private :pointer)
   :5.0+ (m-metadata :pointer)))

#+nil
(cffi:defbitfield ai-post-process-steps
  (:ai-process-calc-tangent-space #x1)
  (:ai-process-join-identical-vertices #x2)
  (:ai-process-make-left-handed #x4)
  (:ai-process-triangulate #x8)
  (:ai-process-remove-component #x10)
  (:ai-process-gen-normals #x20)
  (:ai-process-gen-smooth-normals #x40)
  (:ai-process-split-large-meshes #x80)
  (:ai-process-pre-transform-vertices #x100)
  (:ai-process-limit-bone-weights #x200)
  (:ai-process-validate-data-structure #x400)
  (:ai-process-improve-cache-locality #x800)
  (:ai-process-remove-redundant-materials #x1000)
  (:ai-process-fix-infacing-normals #x2000)
  (:ai-process-sort-by-p-type #x8000)
  (:ai-process-find-degenerates #x10000)
  (:ai-process-find-invalid-data #x20000)
  (:ai-process-gen-uv-coords #x40000)
  (:ai-process-transform-uv-coords #x80000)
  (:ai-process-find-instances #x100000)
  (:ai-process-optimize-meshes #x200000)
  (:ai-process-optimize-graph #x400000)
  (:ai-process-flip-u-vs #x800000)
  (:ai-process-flip-winding-order #x1000000)

  (:ai-process-convert-to-left-handed #x1800004)
  (:ai-process-preset-target-realtime-fast #x4802B)
  (:ai-process-preset-target-realtime-quality #x79ACB))

(cffi:defbitfield ai-post-process-steps ;; 3.0+
  (:ai-process-calc-tangent-space #x1)
  (:ai-process-join-identical-vertices #x2)
  (:ai-process-make-left-handed #x4)
  (:ai-process-triangulate #x8)
  (:ai-process-remove-component #x10)
  (:ai-process-gen-normals #x20)
  (:ai-process-gen-smooth-normals #x40)
  (:ai-process-split-large-meshes #x80)
  (:ai-process-pre-transform-vertices #x100)
  (:ai-process-limit-bone-weights #x200)
  (:ai-process-validate-data-structure #x400)
  (:ai-process-improve-cache-locality #x800)
  (:ai-process-remove-redundant-materials #x1000)
  (:ai-process-fix-infacing-normals #x2000)
  (:ai-process-sort-by-p-type #x8000)
  (:ai-process-find-degenerates #x10000)
  (:ai-process-find-invalid-data #x20000)
  (:ai-process-gen-uv-coords #x40000)
  (:ai-process-transform-uv-coords #x80000)
  (:ai-process-find-instances #x100000)
  (:ai-process-optimize-meshes #x200000)
  (:ai-process-optimize-graph #x400000)
  (:ai-process-flip-u-vs #x800000)
  (:ai-process-flip-winding-order #x1000000)
  (:ai-process-split-by-bone-count #x2000000)
  (:ai-process-debone #x4000000)
  (:ai-process-global-scale #x8000000)        ;; 4.1+
  (:ai-process-embed-textures #x10000000)     ;; 5.0+
  (:ai-process-force-gen-normals #x20000000)  ;; 5.0+
  (:ai-process-drop-normals #x40000000)       ;; 5.0+
  (:ai-process-gen-bounding-boxes #x80000000) ;; 5.0+


  (:ai-process-convert-to-left-handed #x1800004
                                      #-(and) (:ai-process-make-left-handed
                                               :ai-process-flip-u-vs
                                               :ai-process-flip-winding-order))
  (:ai-process-preset-target-realtime-fast #x4802B
                                           #- (and)
                                           (:ai-process-calc-tangent-space
                                            :ai-process-gen-normals
                                            :ai-process-join-identical-vertices
                                            :ai-process-triangulate
                                            :ai-process-gen-uv-coords
                                            :ai-process-sort-by-p-type))
  (:ai-process-preset-target-realtime-quality
   #x79ACB
   #- (and)
   (:ai-process-calc-tangent-space
    :ai-process-gen-smooth-normals
    :ai-process-join-identical-vertices
    :ai-process-improve-cache-locality
    :ai-process-limit-bone-weights
    :ai-process-remove-redundant-materials
    :ai-process-split-large-meshes
    :ai-process-triangulate
    :ai-process-gen-uv-coords
    :ai-process-sort-by-p-type
    :ai-process-find-degenerates
    :ai-process-find-invalid-data))
  (:ai-process-preset-target-realtime-max-quality
   #x379ECB
   #- (and)
   (:ai-process-preset-target-realtime-quality
    :ai-process-find-instances
    :ai-process-validate-data-structure
    :ai-process-optimize-meshes)))


(cffi:defcfun ("aiImportFile" ai-import-file) (:pointer (:struct ai-scene))
  (p-file :string)
  (p-flags ai-post-process-steps))

(cffi:defcstruct ai-uv-transform ;; 3.0+
  (m-translation (:struct ai-vector-2d))
  (m-scaling (:struct ai-vector-2d))
  (m-rotation :float))

(cffi:defcstruct ai-memory-info ;; 3.0+
  (textures :unsigned-int)
  (materials :unsigned-int)
  (meshes :unsigned-int)
  (nodes :unsigned-int)
  (animations :unsigned-int)
  (cameras :unsigned-int)
  (lights :unsigned-int)
  (total :unsigned-int))

(cffi:defcfun ("aiGetMaterialFloatArray" ai-get-material-float-array) ai-return
  (p-mat (:pointer (:struct ai-material)))
  (p-key :string)
  (type :unsigned-int)
  (index :unsigned-int)
  (p-out (:pointer :float))
  (p-max (:pointer :unsigned-int)))

(cffi:defcfun ("aiGetVersionRevision" ai-get-version-revision) :unsigned-int)

#-old-assimp
(cffi:defcfun ("aiIdentityMatrix3" ai-identity-matrix-3) :void
  (mat (:pointer (:struct ai-matrix-3x-3))))

#-old-assimp
(cffi:defcfun ("aiIdentityMatrix4" ai-identity-matrix-4) :void
  (mat (:pointer (:struct ai-matrix-4x-4))))

#-old-assimp
(cffi:defcfun ("aiDetachAllLogStreams" ai-detach-all-log-streams) :void)

;; typedef void (*aiLogStreamCallback)(const char* /* message */, char* /* user */);
(cffi::defctype ai-log-stream-callback :pointer)

(cffi:defcstruct ai-log-stream ;; 3.0+
  (callback ai-log-stream-callback)
  (user (:pointer :char)))

(cffi:defbitfield (ai-default-log-stream :int) ;; 3.0+
  (:ai-default-log-stream-file 1)
  (:ai-default-log-stream-stdout 2)
  (:ai-default-log-stream-stderr 4)
  (:ai-default-log-stream-debugger 8))

#-(or old-assimp (not fsbv))
(cffi:defcfun ("aiGetPredefinedLogStream" ai-get-predefined-log-stream)
    (:struct ai-log-stream)
  (p-streams ai-default-log-stream)
  (file :string))

(cffi:defcenum (ai-shading-mode :int) ;; 3.0+
  (:ai-shading-mode-flat 1)
  (:ai-shading-mode-gouraud 2)
  (:ai-shading-mode-phong 3)
  (:ai-shading-mode-blinn 4)
  (:ai-shading-mode-toon 5)
  (:ai-shading-mode-oren-nayar 6)
  (:ai-shading-mode-minnaert 7)
  (:ai-shading-mode-cook-torrance 8)
  (:ai-shading-mode-no-shading 9)
  (:ai-shading-mode-fresnel 10))

;; typedef size_t (*aiFileReadProc) (struct aiFile*, char*, size_t,size_t);
(cffi::defctype ai-file-read-proc :pointer)

(cffi:defcfun ("aiGetMemoryRequirements" ai-get-memory-requirements) :void
  (p-in (:pointer (:struct ai-scene)))
  (in (:pointer (:struct ai-memory-info))))

;; typedef size_t (*aiFileTellProc) (C_STRUCT aiFile*);
(cffi::defctype ai-file-tell-proc :pointer)

(cffi:defbitfield (ai-component :unsigned-int) ;; 3.0+
  (:ai-component-normals 2)
  (:ai-component-tangents-and-bitangents 4)
  (:ai-component-colors 8)
  (:ai-component-texcoords #x10)
  (:ai-component-boneweights #x20)
  (:ai-component-animations #x40)
  (:ai-component-textures #x80)
  (:ai-component-lights #x100)
  (:ai-component-cameras #x200)
  (:ai-component-meshes #x400)
  (:ai-component-materials #x800)
  ;; probably should get rid of long-name versions?
  (:normals 2)
  (:tangents-and-bitangents 4)
  (:colors 8) ;; all color channels
  (:texcoords #x10) ;; all UV channels
  (:boneweights #x20)
  (:animations #x40)
  (:textures #x80)
  (:lights #x100)
  (:cameras #x200)
  (:meshes #x400)
  (:materials #x800)
  ;; #define aiComponent_COLORSn(n) (1u << (n+20u))
  (:color0 #x200000)
  (:color1 #x400000)
  (:color2 #x800000)
  (:color3 #x1000000)
  (:color4 #x2000000)
  ;; #define aiComponent_TEXCOORDSn(n) (1u << (n+25u))
  (:texcoord0 #x4000000)
  (:texcoord1 #x8000000)
  (:texcoord2 #x10000000)
  (:texcoord3 #x20000000)
  (:texcoord4 #x40000000))

;; typedef struct aiFile* (*aiFileOpenProc) (struct aiFileIO*, const char*, const char*);
(cffi::defctype ai-file-open-proc :pointer)

;; typedef void (*aiFileCloseProc) (struct aiFileIO*, struct aiFile*);
(cffi::defctype ai-file-close-proc :pointer)

(cffi::defctype ai-user-data (:pointer :char))

(cffi:defcstruct ai-file-io ;; 3.0+
  (open-proc ai-file-open-proc)
  (close-proc ai-file-close-proc)
  (user-data ai-user-data))

(cffi:defcenum (ai-texture-mapping :int) ;; 3.0+
  (:ai-texture-mapping-uv 0)
  (:ai-texture-mapping-sphere 1)
  (:ai-texture-mapping-cylinder 2)
  (:ai-texture-mapping-box 3)
  (:ai-texture-mapping-plane 4)
  (:ai-texture-mapping-other 5))

(cffi:defcenum (ai-texture-op :int) ;; 3.0+
  (:ai-texture-op-multiply 0)
  (:ai-texture-op-add 1)
  (:ai-texture-op-subtract 2)
  (:ai-texture-op-divide 3)
  (:ai-texture-op-smooth-add 4)
  (:ai-texture-op-signed-add 5))

(cffi:defcenum (ai-texture-map-mode :int) ;; 3.0+
  (:ai-texture-map-mode-wrap 0)
  (:ai-texture-map-mode-clamp 1)
  (:ai-texture-map-mode-decal 3)
  (:ai-texture-map-mode-mirror 2))

(cffi:defcfun ("aiGetMaterialTexture" ai-get-material-texture) ai-return
  (mat (:pointer (:struct ai-material)))
  (type ai-texture-type)
  (index :unsigned-int)
  (path (:pointer (:struct ai-string)))
  (mapping (:pointer ai-texture-mapping))
  (uvindex (:pointer :unsigned-int))
  (blend (:pointer :float))
  (op (:pointer ai-texture-op))
  (mapmode (:pointer ai-texture-map-mode))
  (flags (:pointer :unsigned-int)))

;; typedef size_t (*aiFileWriteProc) (struct aiFile*, const char*, size_t, size_t);
(cffi::defctype ai-file-write-proc :pointer)

;; typedef enum aiReturn (*aiFileSeek) (struct aiFile*, size_t, enum aiOrigin);
(cffi::defctype ai-file-seek :pointer)

;; typedef void (*aiFileFlushProc) (struct aiFile*);
(cffi::defctype ai-file-flush-proc :pointer)

(cffi:defcstruct ai-file ;; 3.0+
  (read-proc  ai-file-read-proc)
  (write-proc  ai-file-write-proc)
  (tell-proc  ai-file-tell-proc)
  (file-size-proc  ai-file-tell-proc)
  (seek-proc  ai-file-seek)
  (flush-proc ai-file-flush-proc)
  (user-data ai-user-data))

(cffi:defcfun ("aiGetErrorString" ai-get-error-string) :string)

(cffi:defcfun ("aiDecomposeMatrix" ai-decompose-matrix) :void
  (mat (:pointer (:struct ai-matrix-4x-4)))
  (scaling (:pointer (:struct ai-vector-3d)))
  (rotation (:pointer (:struct ai-quaternion)))
  (position (:pointer (:struct ai-vector-3d))))

(cffi:defbitfield (ai-texture-flags :int) ;; 3.0+
  (:ai-texture-flags-invert 1)
  (:ai-texture-flags-use-alpha 2)
  (:ai-texture-flags-ignore-alpha 4))

(cffi:defcfun ("aiGetExtensionList" ai-get-extension-list) :void
  (sz-out (:pointer (:struct ai-string))))

(cffi:defcstruct ai-plane ;; 3.0+
  (a :float)
  (b :float)
  (c :float)
  (d :float))

#-old-assimp
(cffi:defcfun ("aiTransformVecByMatrix3" ai-transform-vec-by-matrix-3) :void
  (vec (:pointer (:struct ai-vector-3d)))
  (mat (:pointer (:struct ai-matrix-3x-3))))

#-old-assimp
(cffi:defcfun ("aiTransformVecByMatrix4" ai-transform-vec-by-matrix-4) :void
  (vec (:pointer (:struct ai-vector-3d)))
  (mat (:pointer (:struct ai-matrix-4x-4))))

#-old-assimp
(cffi:defcfun ("aiDetachLogStream" ai-detach-log-stream) ai-return
  (stream (:pointer (:struct ai-log-stream))))

(cffi:defcfun ("aiSetImportPropertyString" ai-set-import-property-string) :void
  (store :pointer) ;; added in 3.0
  (sz-name :string)
  (st (:pointer (:struct ai-string))))

#-old-assimp
(cffi:defcfun ("aiEnableVerboseLogging" ai-enable-verbose-logging) :void
  (d ai-bool))

#-old-assimp
(cffi:defcfun ("aiAttachLogStream" ai-attach-log-stream) :void
  (stream (:pointer (:struct ai-log-stream))))

(cffi:defcfun ("aiGetMaterialIntegerArray" ai-get-material-integer-array) ai-return
  (p-mat (:pointer (:struct ai-material)))
  (p-key :string)
  (type :unsigned-int)
  (index :unsigned-int)
  (p-out (:pointer :int))
  (p-max (:pointer :unsigned-int)))

(cffi:defcstruct ai-ray ;; 3.0+
  (pos (:struct ai-vector-3d))
  (dir (:struct ai-vector-3d)))

(cffi:defcfun ("aiGetLegalString" ai-get-legal-string) :string)

(cffi:defcfun ("aiGetMaterialColor" ai-get-material-color) ai-return
  (p-mat (:pointer (:struct ai-material)))
  (p-key :string)
  (type :unsigned-int)
  (index :unsigned-int)
  (p-out (:pointer (:struct ai-color-4d))))

(cffi:defcfun ("aiGetCompileFlags" ai-get-compile-flags) :unsigned-int)

(cffi:defcfun ("aiImportFileEx" ai-import-file-ex) (:pointer (:struct ai-scene))
  (p-file :string)
  (p-flags ai-post-process-steps)
  (p-fs (:pointer (:struct ai-file-io))))

;; 3.0
(cffi:defcfun ("aiImportFileExWithProperties" ai-import-file-ex-with-properties)
    (:pointer (:struct ai-scene))
  (p-file :string)
  (p-flags ai-post-process-steps)
  (p-fs (:pointer (:struct ai-file-io)))
  (p-props (:pointer #+(or) (:struct ai-property-store))))

;; 3.0
(cffi:defcfun ("aiImportFileFromMemoryWithProperties" ai-import-file-from-memory-with-properties)
    (:pointer (:struct ai-scene))
  (p-buffer :pointer)
  (p-length :unsigned-int)
  (p-flags ai-post-process-steps)
  (p-hint :string)
  (p-props :pointer))


(cffi:defcfun ("aiGetMaterialString" ai-get-material-string) ai-return
  (p-mat (:pointer (:struct ai-material)))
  (p-key :string)
  (type :unsigned-int)
  (index :unsigned-int)
  (p-out (:pointer (:struct ai-string))))

(cffi:defcenum (ai-blend-mode :int) ;; 3.0+
  (:ai-blend-mode-default 0)
  (:ai-blend-mode-additive 1))

(cffi:defcfun ("aiSetImportPropertyInteger" ai-set-import-property-integer) :void
  (store :pointer) ;; added in 3.0
  (sz-name :string)
  (value :int))

(cffi:defcfun ("aiReleaseImport" ai-release-import) :void
  (p-scene (:pointer (:struct ai-scene))))

#-old-assimp
(cffi:defcfun ("aiTransposeMatrix3" ai-transpose-matrix-3) :void
  (mat (:pointer (:struct ai-matrix-3x-3))))

#-old-assimp
(cffi:defcfun ("aiTransposeMatrix4" ai-transpose-matrix-4) :void
  (mat (:pointer (:struct ai-matrix-4x-4))))

#-old-assimp
(cffi:defcfun ("aiMultiplyMatrix3" ai-multiply-matrix-3) :void
  (dst (:pointer (:struct ai-matrix-3x-3)))
  (src (:pointer (:struct ai-matrix-3x-3))))

#-old-assimp
(cffi:defcfun ("aiMultiplyMatrix4" ai-multiply-matrix-4) :void
  (dst (:pointer (:struct ai-matrix-4x-4)))
  (src (:pointer (:struct ai-matrix-4x-4))))

(cffi:defbitfield (ai-primitive-type :unsigned-int) ;; 3.0+
  (:ai-primitive-type-point 1)
  (:ai-primitive-type-line 2)
  (:ai-primitive-type-triangle 4)
  (:ai-primitive-type-polygon 8)
  ;; probably should get rid of long-name versions?
  (:point 1)
  (:line 2)
  (:triangle 4)
  (:polygon 8))

#-old-assimp
(cffi:defcfun ("aiApplyPostProcessing" ai-apply-post-processing)
    (:pointer (:struct ai-scene))
  (p-scene (:pointer (:struct ai-scene)))
  (p-flags :unsigned-int))

#-old-assimp
(cffi:defcfun ("aiGetMaterialTextureCount" ai-get-material-texture-count) :unsigned-int
  (p-mat (:pointer (:struct ai-material)))
  (type ai-texture-type))

#-old-assimp
(cffi:defcfun ("aiImportFileFromMemory" ai-import-file-from-memory)
    (:pointer (:struct ai-scene))
  (p-buffer :pointer)
  (p-length :unsigned-int)
  (p-flags ai-post-process-steps)
  (p-hint :pointer))

;; 3.0
(cffi:defcfun ("aiCreatePropertyStore" ai-create-property-store) :pointer)

;; 3.0
(cffi:defcfun ("aiReleasePropertyStore" ai-release-property-store) :void
  (p :pointer))

;; 3.1+
(cffi:defcfun ("aiSetImportPropertyMatrix" ai-set-import-property-matrix)
    :void
  (store :pointer)
  (sz-name :string)
  (mat (:pointer (:struct ai-matrix-4x-4))))

(cffi:defcfun ("aiGetMaterialUVTransform" ai-get-material-uv-transform)
    ai-return
  (p-mat (:pointer (:struct ai-material)))
  (p-key :string)
  (type :unsigned-int)
  (index :unsigned-int)
  (p-out (:pointer (:struct ai-uv-transform))))

;; 3.2+
(cffi:defbitfield ai-importer-flags ;; 3.2+
  (:text 1)
  (:binary 2)
  (:compressed 4)
  (:limited-support 8)
  (:experimental #x10))

(cffi:defcstruct ai-importer-desc ;; 3.2+
  (m-name :string)
  (m-author :string)
  (m-maintainer :string)
  (m-comments :string)
  (m-flags ai-importer-flags)
  (m-min-major :unsigned-int)
  (m-min-minor :unsigned-int)
  (m-max-major :unsigned-int)
  (m-max-minor :unsigned-int)
  (m-file-extensions :string))

(cffi:defcfun ("aiGetImportFormatCount" ai-get-import-format-count) size-t)

(cffi:defcfun ("aiGetImportFormatDescription" ai-get-import-format-description)
    (:pointer (:struct ai-importer-desc))
  (p-index size-t))

(cffi:defcfun ("aiGetImporterDesc" ai-get-importer-description)
    (:pointer (:struct ai-importer-desc))
  (extension :string))

;; used by properties
(cffi:defbitfield ai-uvtrafo
  (:scaling 1)
  (:rotation 2)
  (:translation 4)
  (:scale 1)
  (:rotate 2)
  (:translate 4))



;; todo: export support
;; aiCopyScene
;; aiExportScene
;; aiExportSceneEx
;; aiExportSceneToBlob
;; aiFreeScene
;; aiGetExportFormatCount
;; aiGetExportFormatDescription
;; aiReleaseExportBlob
;; aiReleaseExportFormatDescription
