(in-package #:classimp)

;;; PROPERTIES argument to importer should be a plist like
;;  '(:pp-sbp-remove (:point :line))
;;; which configures the :ai-process-sort-by-p-type postprocess step
;;; to remove meshes with :line or :point primitives

;;; summaries of the available properties are below, see
;;; include/assimp/config.h of C lib for full details.

;; helpers for translating AI_CONFIG_* and setting import properties
(defparameter *ai-config-map*
  (alexandria:alist-hash-table
   (loop
     for (name . type)
       in
     '(
       ;; enable timing stats for loader, default false
       (:glob-measure-time . :bool)

       ;; don't generate a dummy skeleton for anim-only files, default false
       (:import-no-skeleton-meshes . :bool)

       ;; not implemented?
       ;; control multithread policy, default -1 (auto)
       ;; 0=disable, >=1 = max # of threads for loader
       (:glob-multithreading . :int)

       ;; favor speed over quality in importer if possible, default false
       (:favour-speed . :bool)          ; int?

       ;; tell fbx importer whether to merge all geometry layers or
       ;; only use first. default true (= merge all)
       (:import-fbx-read-all-geometry-layers . :bool)

       ;; tell fbx importer to include unused materials. default false
       (:import-fbx-read-all-materials . :bool)

       ;; tell fbx importer to read materials. default true
       (:import-fbx-read-materials . :bool)

       ;; tell fbx importer to read textures. default true
       (:import-fbx-read-textures . :bool)

       ;; tell fbx importer to read cameras. default true
       (:import-fbx-read-cameras . :bool)

       ;; tell fbx importer to read lights. default true
       (:import-fbx-read-lights . :bool)

       ;; tell fbx importer to read animations. default true
       (:import-fbx-read-animations . :bool)

       ;; limit fbx importer to FBX 2013 format (best supported and
       ;; tested). default false
       (:import-fbx-strict-mode . :bool)

       ;; tell fbx importer whether to add nodes for pivot points or
       ;; evaluate them when possible. default true
       (:import-fbx-preserve-pivots . :bool)

       ;; tell fbx to drop anim curves that are empty or match bind
       ;; pose over whole range. default true
       (:import-fbx-optimize-empty-animation-curves . :bool)

       ;; for formats with vertex anims and loaders that don't support
       ;; it, specify which keyframe to load. default 0
       (:import-global-keyframe . :int)

       ;; same for specific formats:
       (:import-md2-keyframe . :int)
       (:import-md3-keyframe . :int)
       (:import-mdc-keyframe . :int)
       (:import-mdl-keyframe . :int)
       (:import-smd-keyframe . :int)
       (:import-unreal-keyframe . :int)

       ;; tell AC loader to move surfaces with "backface cull" flag to
       ;; separate meshes. default true
       (:import-ac-separate-bfcull . :bool)

       ;; tell AC loader to evaluate subdivision surfaces. default true
       (:import-ac-eval-subdivision . :bool)

       ;; tell unreal loader to separate faces with different
       ;; flags. default true
       (:import-unreal-handle-flags . :bool)

       ;; tell terragen loader to compute UVs when not provided and
       ;; assign default texture. default false
       (:import-ter-make-uvs . :bool)

       ;; tell ASE loader to always generate normals from smoothing
       ;; groups. default true
       (:import-ase-reconstruct-normals . :bool)

       ;; tell md3 importer to detect and process multipart models and
       ;; combine into one. default true
       (:import-md3-handle-multipart . :bool)

       ;; name combined with md3 filename to generate a skin name
       ;; ('default', 'red', 'blue', etc). default "default"
       (:import-md3-skin-name . :string)

       ;; filename or path used for loading shader files for
       ;; md3 loader. default ""
       (:import-md3-shader-src . :string)

       ;; tell LWO loader to only load single layer with given name or
       ;; index. defaults to loading all layers
       (:import-lwo-one-layer-only . :int/string)

       ;; tell md5 loader not to automatically load associated md5anim
       ;; file when loading md5mesh. default false
       (:import-md5-no-anim-autoload . :bool)

       ;; specify time range (in frames) to use when evaluating
       ;; animations in LWS loader. defaults to values from file
       (:import-lws-anim-start . :int)
       (:import-lws-anim-end . :int)

       ;; specify output framerate for IRR loader. default 100
       (:import-irr-anim-fps . :int)

       ;; tell Ogre loader to look for materials in specified file if
       ;; not found in file with same base name as material or
       ;; mesh. default "Scene.material"
       (:import-ogre-material-file . :string)

       ;; tell Ogre loader to look at texture filename to determine
       ;; usage instead of looking at texture unit
       ;; name. (ex. foo_normal, foo_spec, etc). default false
       (:import-ogre-texturetype-from-filename . :bool)

       ;; default false
       (:android-jni-assimp-manager-support . :bool)

       ;; tell IFC loader to skip IfcSpace elements. default true
       (:import-ifc-skip-space-representations . :bool)

       ;; tell IFC loader to triangulate mesh, since post-process
       ;; triangulate doesn't handle its output well. default true
       (:import-ifc-custom-triangulation . :bool)

       ;; set smoothing angle for IFC smoothing curves. default 10,
       ;; range=5.0-120
       (:import-ifc-smoothing-angle . :float)

       ;; set tessellation for cylinders, default 32, range 3-180
       (:import-ifc-cylindrical-tessellation . :int)

       ;;???
       (:import-ifc-skip-curve-representations . :bool)

       ;; tell collada loader to ignore "up" direction from
       ;; file. default false
       (:import-collada-ignore-up-direction . :bool)

       ;; tell X file exporter to use doubles instead of single floats
       (:export-xfile-64bit . :bool)

       ;; path to file containing colormap for textures in MDL files
       ;; default "colormap.lmp"
       (:import-mdl-colormap . :string)

       ;; smoothing angle in degrees for
       ;; :ai-process-calc-tangent-space. default 45, max 175
       (:pp-ct-max-smoothing-angle . :float)

       ;; UV channel containing data used in tangent space calculation
       ;; for :ai-process-calc-tangent-space (default 0)
       (:pp-ct-texture-channel-index . :int)

       ;; require all bones qualify for deboning before removing any
       ;; in :ai-process-debone default false
       (:pp-db-all-or-none . :bool)

       ;; threshold for :ai-process-debone, default 1.0
       (:pp-db-threshold . :float)

       ;; remove degenerate primitives directly instead of converting
       ;; to lines/points in :ai-process-find-degenerates. default
       ;; false
       (:pp-fd-remove . :bool)

       ;; epsilon used for comparing key values when removing
       ;; animations with no changes in values in
       ;; :ai-process-find-invalid-data. default 0.0
       (:pp-fid-anim-accuracy . :float)

       ;; smoothing angle in degrees for
       ;; :ai-process-gen-smooth-normals. default/max 175
       (:pp-gsn-max-smoothing-angle . :float)

       ;; size (in vertices) of post-transform vertex cache to
       ;; optimize for in :ai-process-improve-cache-locality. default
       ;; 12
       (:pp-icl-ptcache-size . :int)

       ;; max # of bones affecting a vertex for
       ;; :ai-process-limit-bone-weights. default 4
       (:pp-lbw-max-weights . :int)

       ;; string containing space separated names of nodes to preserve
       ;; in :ai-process-optimize-graph. default ""
       (:pp-og-exclude-list . :string)

       ;; move meshes to world space but keep scene hierarchy in
       ;; :ai-process-pre-transform-vertices. default false
       (:pp-ptv-keep-hierarchy . :bool)

       ;; scale meshes to [-1,1] while transforming meshes in
       ;; :ai-process-pre-transform-vertices. default false?
       (:pp-ptv-normalize . :bool)

       ;; use matrix specified by :pp-ptv-root-transformation as scene
       ;; root in :ai-process-pre-transform-vertices. default false
       #++(:PP-PTV-ADD-ROOT-TRANSFORMATION . :bool)

       ;; root transform for :ai-process-pre-transform-vertices. default ?
       #++(:PP-PTV-ROOT-TRANSFORMATION . :mat4)

       ;; string containing space separated names of materials to keep
       ;; for :ai-process-remove-redundant-materials. default "", use
       ;; '' to include spaces in names
       (:pp-rrm-exclude-list . :string)

       ;; components (:normals, :colors, :lights etc) to remove in
       ;; :ai-process-remove-component. default none
       (:pp-rvc-flags . (:bitfield %ai:ai-component))

       ;; max bones per mesh with :ai-process-split-by-bone-count (default 60)
       (:pp-sbbc-max-bones . :int)

       ;; primitive types (:point :line :triangle :polygon) to remove
       ;; from meshes in :ai-process-sort-by-p-type, default none
       (:pp-sbp-remove . (:bitfield %ai:ai-primitive-type))

       ;; max # of triangles in mesh for
       ;; :ai-process-split-large-meshes. default 1000000
       (:pp-slm-triangle-limit . :int)

       ;; max # of vertices in mesh for
       ;; :ai-process-split-large-meshes. default 1000000
       (:pp-slm-vertex-limit . :int)

       ;; which transforms to process in
       ;; :ai-process-transform-uv-coords. default (:scale :rotate
       ;; :transform) = all
       (:pp-tuv-evaluate . (:bitfield ai-uvtrafo)))
     collect (list name type
                   (string-upcase
                    (substitute #\_ #\- (symbol-name name)))))))

(defmacro with-property-store ((store &key properties) &body body)
  (alexandria:with-gensyms (prop val)
    `(let ((,store (%ai:ai-create-property-store)))
       (unwind-protect
            (progn
              ,@(when properties
                  `((loop for (,prop ,val) on ,properties by #'cddr
                          do (set-import-property* ,store ,prop ,val))))
              ,@body)
         (%ai:ai-release-property-store ,store)))))

(defun set-import-property* (store prop value)
  (destructuring-bind (type name) (gethash prop *ai-config-map*)
    (unless (and name type)
      (error "don't know how to set import property ~s?" prop))
    (cond
      ((eql type :int)
       (%ai:ai-set-import-property-integer store name value))
      ((eql type :bool)
       (%ai:ai-set-import-property-integer store name
                                           (cond ((eql value t) 1)
                                                 ((eql value nil) 0)
                                                 ;; allow passing 0/1 directly
                                                 (t value))))
      ((eql type :string)
       (%ai:ai-set-import-property-string store name value))
      ((eql type :int/string)
       (etypecase value
         (integer
          (%ai:ai-set-import-property-integer store name value))
         (string
          (%ai:ai-set-import-property-string store name value))))
      ((eql type :float)
       (%ai:ai-set-import-property-float store name (float value 1.0)))
      ((and (consp type) (eql (car type) :enum))
       (let* ((enum (second type)))
         (flet ((v (a)
                  (if (numberp a)
                      a
                      (cffi:foreign-enum-value enum a))))
           (%ai:ai-set-import-property-integer store name
                                               (if (consp value)
                                                   (reduce 'logior
                                                           (mapcar #'v value))
                                                   (v value))))))
      ((and (consp type) (eql (car type) :bitfield))
       (let* ((bitfield (second type)))
         (flet ((v (a)
                  (if (numberp a)
                      a
                      (cffi:foreign-bitfield-value bitfield a))))
           (%ai:ai-set-import-property-integer store name
                                               (v value)))))
      (t (error "don't know how to set import property ~s (type ~s?)"
                prop type)))))
