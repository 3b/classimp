(cl:in-package :%open-asset-import-library)

;; long is not always equal to size_t, long is 4 bytes on Windows 64
#+cffi-features:x86-64 (cffi::defctype size-t :unsigned-long-long)
#-cffi-features:x86-64 (cffi::defctype size-t :unsigned-long)

(cffi:defcstruct ai-string
  (length size-t)
  (data :char :count 1024))

(cffi:defcstruct ai-string32
  (length :uint32)
  (data :char :count 1024))

(cffi:defcstruct ai-vector-3d
  (x :float)
  (y :float)
  (z :float))

(cffi:defcstruct ai-camera
  (m-name (:struct ai-string))
  (m-position (:struct ai-vector-3d))
  (m-up (:struct ai-vector-3d))
  (m-look-at (:struct ai-vector-3d))
  (m-horizontal-fov :float)
  (m-clip-plane-near :float)
  (m-clip-plane-far :float)
  (m-aspect :float))

(cffi:defcenum (ai-origin :int)
  (:ai-origin-set 0)
  (:ai-origin-cur 1)
  (:ai-origin-end 2))

(cffi:defcstruct ai-color-3d
  (r :float)
  (g :float)
  (b :float))

(cffi:defcenum (ai-property-type-info :int)
  (:ai-pti-float 1)
  (:ai-pti-string 3)
  (:ai-pti-integer 4)
  (:ai-pti-buffer 5))

(cffi::defctype ai-bool :int)

(cffi:defcfun ("aiIsExtensionSupported" ai-is-extension-supported) ai-bool
  (sz-extension :pointer))

(cffi:defcstruct ai-vector-2d
  (x :float)
  (y :float))

(cffi:defcenum (ai-return :int)
  (:ai-return-success 0)
  (:ai-return-failure -1)
  (:ai-return-outofmemory -3))

(cffi:defcstruct ai-vector-key
  (m-time :double)
  (m-value (:struct ai-vector-3d)))

(cffi:defcstruct ai-quaternion
  (w :float)
  (x :float)
  (y :float)
  (z :float))

(cffi:defcstruct ai-quat-key
  (m-time :double)
  (m-value (:struct ai-quaternion)))

(cffi:defcenum (ai-anim-behaviour :unsigned-int)
  (:ai-anim-behaviour-default 0)
  (:ai-anim-behaviour-constant 1)
  (:ai-anim-behaviour-linear 2)
  (:ai-anim-behaviour-repeat 3))

(cffi:defcstruct ai-node-anim
  (m-node-name (:struct ai-string))
  (m-num-position-keys :unsigned-int)
  (m-position-keys (:pointer (:struct ai-vector-key)))
  (m-num-rotation-keys :unsigned-int)
  (m-rotation-keys (:pointer (:struct ai-quat-key)))
  (m-num-scaling-keys :unsigned-int)
  (m-scaling-keys (:pointer (:struct ai-vector-key)))
  (m-pre-state ai-anim-behaviour)
  (m-post-state ai-anim-behaviour))

(cffi:defcstruct ai-animation
  (m-name (:struct ai-string))
  (m-duration :double)
  (m-ticks-per-second :double)
  (m-num-channels :unsigned-int)
  (m-channels (:pointer (:pointer (:struct ai-node-anim)))))

(cffi:defcstruct ai-matrix-3x-3
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
  (quat :pointer)
  (mat :pointer))

(cffi:defcenum (ai-texture-type :int)
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
  (:ai-texture-type-unknown 12))

(cffi:defcstruct ai-material-property
  (m-key (:struct ai-string))
  (m-semantic ai-texture-type)
  (m-index :unsigned-int)
  (m-data-length :unsigned-int)
  (m-type ai-property-type-info)
  (m-data (:pointer :char)))

(cffi:defcfun ("aiSetImportPropertyFloat" ai-set-import-property-float) :void
  (store :pointer) ;; added in 3.0
  (sz-name :pointer)
  (value :float))

(cffi:defcstruct ai-material
  (m-properties (:pointer (:pointer (:struct ai-material-property))))
  (m-num-properties :unsigned-int)
  (m-num-allocated :unsigned-int))

#-old-assimp
(cffi:defcfun ("aiGetMaterialProperty" ai-get-material-property) ai-return
  (p-mat :pointer)
  (p-key :pointer)
  (type ai-texture-type)
  (index :unsigned-int)
  (p-prop-out :pointer))

(cffi:defcstruct ai-matrix-4x-4
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

(cffi:defcstruct ai-node
  (m-name (:struct ai-string))
  (m-transformation (:struct ai-matrix-4x-4))
  (m-parent :pointer)
  (m-num-children :unsigned-int)
  (m-children :pointer)
  (m-num-meshes :unsigned-int)
  (m-meshes (:pointer :unsigned-int)))

(cffi:defcstruct ai-color-4d
  (r :float)
  (g :float)
  (b :float)
  (a :float))

(cffi:defcstruct ai-face
  (m-num-indices :unsigned-int)
  (m-indices (:pointer :unsigned-int)))

(cffi:defcstruct ai-vertex-weight
  (m-vertex-id :unsigned-int)
  (m-weight :float))

(cffi:defcstruct ai-bone
  (m-name (:struct ai-string))
  (m-num-weights :unsigned-int)
  (m-weights (:pointer (:struct ai-vertex-weight)))
  (m-offset-matrix (:struct ai-matrix-4x-4)))

;; fixme: probably should grovel these, aiMesh.h says they shouldn't change
;; though, so ignoring for now...
;; ... they changed anyway :/
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defconstant +ai-max-number-of-color-sets+ 8)
  (cl:defconstant +ai-max-number-of-texturecoords+ 8))

;; 3.0 assimp doco says this is unused unused
#++
(cffi:defcstruct ai-anim-mesh
  (m-vertices :pointer)
  (m-normals :pointer)
  (m-tangents :pointer)
  (m-bitangents :pointer)
  (m-colors :pointer :count #.+ai-max-number-of-color-sets+)
  (m-texture-coords :pointer :count #.+ai-max-number-of-texturecoords+)
  (m-num-vertices :unsigned-int))

(cffi:defcstruct ai-mesh
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
  (m-name (:struct ai-string)) ;; 3.0
  (m-num-anim-meshes :unsigned-int) ;; 3.0, unused
  (m-anim-meshes :pointer)) ;; 3.0 unused

(cffi:defcstruct ai-texel
  (b :unsigned-char)
  (g :unsigned-char)
  (r :unsigned-char)
  (a :unsigned-char))

(cffi:defcstruct ai-texture
  (m-width :unsigned-int)
  (m-height :unsigned-int)
  (ach-format-hint :char :count 4)
  (pc-data (:pointer (:struct ai-texel))))

(cffi:defcenum (ai-light-source-type :int)
  (:ai-light-source-undefined 0)
  (:ai-light-source-directional 1)
  (:ai-light-source-point 2)
  (:ai-light-source-spot 3))

(cffi:defcstruct ai-light
  (m-name (:struct ai-string))
  (m-type ai-light-source-type)
  (m-position (:struct ai-vector-3d))
  (m-direction (:struct ai-vector-3d))
  (m-attenuation-constant :float)
  (m-attenuation-linear :float)
  (m-attenuation-quadratic :float)
  (m-color-diffuse (:struct ai-color-3d))
  (m-color-specular (:struct ai-color-3d))
  (m-color-ambient (:struct ai-color-3d))
  (m-angle-inner-cone :float)
  (m-angle-outer-cone :float))

(cffi:defcstruct ai-scene
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
  (m-private :pointer)) ;; 3.0

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
  (:ai-process-preset-target-realtime-quality #x79ACB)

  )

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


(cffi:defcfun ("aiImportFile" ai-import-file) :pointer
  (p-file :string)
  (p-flags ai-post-process-steps))

(cffi:defcstruct ai-uv-transform
  (m-translation (:struct ai-vector-2d))
  (m-scaling (:struct ai-vector-2d))
  (m-rotation :float))

(cffi:defcstruct ai-memory-info
  (textures :unsigned-int)
  (materials :unsigned-int)
  (meshes :unsigned-int)
  (nodes :unsigned-int)
  (animations :unsigned-int)
  (cameras :unsigned-int)
  (lights :unsigned-int)
  (total :unsigned-int))

(cffi:defcfun ("aiGetMaterialFloatArray" ai-get-material-float-array) ai-return
  (p-mat :pointer)
  (p-key :pointer)
  (type :unsigned-int)
  (index :unsigned-int)
  (p-out (:pointer :float))
  (p-max (:pointer :unsigned-int)))

(cffi:defcfun ("aiGetVersionRevision" ai-get-version-revision) :unsigned-int)

#-old-assimp
(cffi:defcfun ("aiIdentityMatrix3" ai-identity-matrix-3) :void (mat :pointer))

#-old-assimp
(cffi:defcfun ("aiIdentityMatrix4" ai-identity-matrix-4) :void (mat :pointer))

#-old-assimp
(cffi:defcfun ("aiDetachAllLogStreams" ai-detach-all-log-streams) :void)

(cffi::defctype ai-log-stream-callback :pointer)

(cffi:defcstruct ai-log-stream
  (callback ai-log-stream-callback)
  (user (:pointer :char)))

(cffi:defbitfield (ai-default-log-stream :int)
  (:ai-default-log-stream-file 1)
  (:ai-default-log-stream-stdout 2)
  (:ai-default-log-stream-stderr 4)
  (:ai-default-log-stream-debugger 8))

#-old-assimp
(cffi:defcfun ("aiGetPredefinedLogStream" ai-get-predefined-log-stream)
    ai-log-stream
  (p-streams ai-default-log-stream)
  (file :string))

(cffi:defcenum (ai-shading-mode :int)
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

(cffi::defctype ai-file-read-proc :pointer)

(cffi:defcfun ("aiGetMemoryRequirements" ai-get-memory-requirements) :void
  (p-in :pointer)
  (in :pointer))

(cffi::defctype ai-file-tell-proc :pointer)

(cffi:defcenum (ai-component :unsigned-int)
  (:ai-component-normals 2)
  (:ai-component-tangents-and-bitangents 4)
  (:ai-component-colors 8)
  (:ai-component-texcoords 16)
  (:ai-component-boneweights 32)
  (:ai-component-animations 64)
  (:ai-component-textures 128)
  (:ai-component-lights 256)
  (:ai-component-cameras 512)
  (:ai-component-meshes 1024)
  (:ai-component-materials 2048))

(cffi::defctype ai-file-open-proc :pointer)

(cffi::defctype ai-file-close-proc :pointer)

(cffi::defctype ai-user-data (:pointer :char))

(cffi:defcstruct ai-file-io
  (open-proc ai-file-open-proc)
  (close-proc ai-file-close-proc)
  (user-data ai-user-data))

(cffi:defcenum (ai-texture-mapping :int)
  (:ai-texture-mapping-uv 0)
  (:ai-texture-mapping-sphere 1)
  (:ai-texture-mapping-cylinder 2)
  (:ai-texture-mapping-box 3)
  (:ai-texture-mapping-plane 4)
  (:ai-texture-mapping-other 5))

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
  (:ai-texture-map-mode-decal 3)
  (:ai-texture-map-mode-mirror 2))

(cffi:defcfun ("aiGetMaterialTexture" ai-get-material-texture) ai-return
  (mat :pointer)
  (type ai-texture-type)
  (index :unsigned-int)
  (path :pointer)
  (mapping :pointer)
  (uvindex (:pointer :unsigned-int))
  (blend (:pointer :float))
  (op :pointer)
  (mapmode :pointer)
  (flags (:pointer :unsigned-int)))

(cffi::defctype ai-file-write-proc :pointer)

(cffi::defctype ai-file-seek :pointer)

(cffi::defctype ai-file-flush-proc :pointer)

(cffi:defcstruct ai-file
  (read-proc  ai-file-read-proc)
  (write-proc  ai-file-write-proc)
  (tell-proc  ai-file-tell-proc)
  (file-size-proc  ai-file-tell-proc)
  (seek-proc  ai-file-seek)
  (flush-proc ai-file-flush-proc)
  (user-data ai-user-data))

(cffi:defcfun ("aiGetErrorString" ai-get-error-string) :pointer)

(cffi:defcfun ("aiDecomposeMatrix" ai-decompose-matrix) :void (mat :pointer)
              (scaling :pointer)
              (rotation :pointer)
              (position :pointer))

(cffi:defbitfield (ai-texture-flags :int)
  (:ai-texture-flags-invert 1)
  (:ai-texture-flags-use-alpha 2)
  (:ai-texture-flags-ignore-alpha 4))

(cffi:defcfun ("aiGetExtensionList" ai-get-extension-list) :void
  (sz-out :pointer))

(cffi:defcfun ("aiGetVersionMinor" ai-get-version-minor) :unsigned-int)

(cffi:defcstruct ai-plane
  (a :float)
  (b :float)
  (c :float)
  (d :float))

#-old-assimp
(cffi:defcfun ("aiTransformVecByMatrix3" ai-transform-vec-by-matrix-3) :void
  (vec :pointer)
  (mat :pointer))

#-old-assimp
(cffi:defcfun ("aiTransformVecByMatrix4" ai-transform-vec-by-matrix-4) :void
  (vec :pointer)
  (mat :pointer))

#-old-assimp
(cffi:defcfun ("aiDetachLogStream" ai-detach-log-stream) ai-return
  (stream :pointer))

(cffi:defcfun ("aiGetVersionMajor" ai-get-version-major) :unsigned-int)

(cffi:defcfun ("aiSetImportPropertyString" ai-set-import-property-string) :void
  (store :pointer) ;; added in 3.0
  (sz-name :pointer)
  (st :pointer))

#-old-assimp
(cffi:defcfun ("aiEnableVerboseLogging" ai-enable-verbose-logging) :void
  (d ai-bool))

#-old-assimp
(cffi:defcfun ("aiAttachLogStream" ai-attach-log-stream) :void (stream :pointer))

(cffi:defcfun ("aiGetMaterialIntegerArray" ai-get-material-integer-array) ai-return
  (p-mat :pointer)
  (p-key :pointer)
  (type :unsigned-int)
  (index :unsigned-int)
  (p-out (:pointer :int))
  (p-max (:pointer :unsigned-int)))

(cffi:defcstruct ai-ray
  (pos (:struct ai-vector-3d))
  (dir (:struct ai-vector-3d)))

(cffi:defcfun ("aiGetLegalString" ai-get-legal-string) :pointer)

(cffi:defcfun ("aiGetMaterialColor" ai-get-material-color) ai-return
  (p-mat :pointer)
  (p-key :pointer)
  (type :unsigned-int)
  (index :unsigned-int)
  (p-out :pointer))

(cffi:defcfun ("aiGetCompileFlags" ai-get-compile-flags) :unsigned-int)

(cffi:defcfun ("aiImportFileEx" ai-import-file-ex) :pointer
  (p-file :pointer)
  (p-flags ai-post-process-steps)
  (p-fs :pointer))

;; 3.0
(cffi:defcfun ("aiImportFileExWithProperties" ai-import-file-ex-with-properties) :pointer
  (p-file :pointer)
  (p-flags ai-post-process-steps)
  (p-fs :pointer)
  (p-props :pointer))

;; 3.0
(cffi:defcfun ("aiImportFileFromMemoryWithProperties" ai-import-file-from-memory-with-properties) :pointer
  (p-buffer :pointer)
  (p-length :unsigned-int)
  (p-flags ai-post-process-steps)
  (p-hint :string)
  (p-props :pointer))


(cffi:defcfun ("aiGetMaterialString" ai-get-material-string) ai-return
  (p-mat :pointer)
  (p-key :pointer)
  (type :unsigned-int)
  (index :unsigned-int)
  (p-out :pointer))

(cffi:defcenum (ai-blend-mode :int)
  (:ai-blend-mode-default 0)
  (:ai-blend-mode-additive 1))

(cffi:defcfun ("aiSetImportPropertyInteger" ai-set-import-property-integer) :void
  (store :pointer) ;; added in 3.0
  (sz-name :pointer)
  (value :int))

(cffi:defcfun ("aiReleaseImport" ai-release-import) :void (p-scene :pointer))

#-old-assimp
(cffi:defcfun ("aiTransposeMatrix3" ai-transpose-matrix-3) :void (mat :pointer))

#-old-assimp
(cffi:defcfun ("aiTransposeMatrix4" ai-transpose-matrix-4) :void (mat :pointer))

#-old-assimp
(cffi:defcfun ("aiMultiplyMatrix3" ai-multiply-matrix-3) :void (dst :pointer)
              (src :pointer))

#-old-assimp
(cffi:defcfun ("aiMultiplyMatrix4" ai-multiply-matrix-4) :void (dst :pointer)
              (src :pointer))

(cffi:defcenum (ai-primitive-type :unsigned-int)
  (:ai-primitive-type-point 1)
  (:ai-primitive-type-line 2)
  (:ai-primitive-type-triangle 4)
  (:ai-primitive-type-polygon 8))

#-old-assimp
(cffi:defcfun ("aiApplyPostProcessing" ai-apply-post-processing) :pointer
  (p-scene :pointer)
  (p-flags :unsigned-int))

#-old-assimp
(cffi:defcfun ("aiGetMaterialTextureCount" ai-get-material-texture-count) :unsigned-int
  (p-mat :pointer)
  (type ai-texture-type))

#-old-assimp
(cffi:defcfun ("aiImportFileFromMemory" ai-import-file-from-memory) :pointer
  (p-buffer :pointer)
  (p-length :unsigned-int)
  (p-flags ai-post-process-steps)
  (p-hint :pointer))

;; 3.0
(cffi:defcfun ("aiCreatePropertyStore" ai-create-property-store) :pointer
  )

;; 3.0
(cffi:defcfun ("aiReleasePropertyStore" ai-release-property-store) :void
  (p :pointer))
