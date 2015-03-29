(in-package #:classimp)

;; helpers for translating AI_CONFIG_* and setting import properties
(defparameter *ai-config-map*
  (alexandria:alist-hash-table
   (loop for (name . type) in '((:FAVOUR-SPEED . :bool) ;; int?
                                (:GLOB-MEASURE-TIME . :bool)
                                (:IMPORT-AC-EVAL-SUBDIVISION . :bool)
                                (:IMPORT-AC-SEPARATE-BFCULL . :bool)
                                (:IMPORT-ASE-RECONSTRUCT-NORMALS . :bool)
                                (:IMPORT-GLOBAL-KEYFRAME . :int)
                                (:IMPORT-IFC-CUSTOM-TRIANGULATION . :bool)
                                (:IMPORT-IFC-SKIP-CURVE-REPRESENTATIONS . :bool)
                                (:IMPORT-IFC-SKIP-SPACE-REPRESENTATIONS . :bool)
                                (:IMPORT-IRR-ANIM-FPS . :int)
                                (:IMPORT-LWO-ONE-LAYER-ONLY . :int)
                                (:IMPORT-LWS-ANIM-END . :int)
                                (:IMPORT-LWS-ANIM-START . :int)
                                (:IMPORT-MD2-KEYFRAME . :int)
                                (:IMPORT-MD3-HANDLE-MULTIPART . :bool)
                                (:IMPORT-MD3-KEYFRAME . :int)
                                (:IMPORT-MD3-SHADER-SRC . :string)
                                (:IMPORT-MD3-SKIN-NAME . :string)
                                (:IMPORT-MD5-NO-ANIM-AUTOLOAD . :bool)
                                (:IMPORT-MDC-KEYFRAME . :int)
                                (:IMPORT-MDL-COLORMAP . :string)
                                (:IMPORT-MDL-KEYFRAME . :int)
                                (:IMPORT-OGRE-MATERIAL-FILE . :string)
                                (:IMPORT-OGRE-TEXTURETYPE-FROM-FILENAME . :bool)
                                (:IMPORT-SMD-KEYFRAME . :int)
                                (:IMPORT-TER-MAKE-UVS . :bool)
                                (:IMPORT-UNREAL-HANDLE-FLAGS . :bool)
                                (:IMPORT-UNREAL-KEYFRAME . :int)
                                (:PP-CT-MAX-SMOOTHING-ANGLE . :float)
                                (:PP-CT-TEXTURE-CHANNEL-INDEX . :int)
                                (:PP-DB-ALL-OR-NONE . :bool)
                                (:PP-DB-THRESHOLD . :float)
                                (:PP-FD-REMOVE . :bool)
                                (:PP-FID-ANIM-ACCURACY . :float)
                                (:PP-GSN-MAX-SMOOTHING-ANGLE . :float)
                                (:PP-ICL-PTCACHE-SIZE . :int)
                                (:PP-LBW-MAX-WEIGHTS . :int)
                                (:PP-OG-EXCLUDE-LIST . :string)
                                (:PP-PTV-KEEP-HIERARCHY . :bool)
                                (:PP-PTV-NORMALIZE . :bool)
                                (:PP-RRM-EXCLUDE-LIST . :string)
                                (:PP-RVC-FLAGS . (:enum %ai:ai-component))
                                (:PP-SBBC-MAX-BONES . :int)
                                (:PP-SBP-REMOVE . (:enum %ai:ai-primitive-type))
                                (:PP-SLM-TRIANGLE-LIMIT . :int)
                                (:PP-SLM-VERTEX-LIMIT . :int)
                                (:PP-TUV-EVALUATE . (:enum :ai-uvtrafo)))
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
      (t (error "don't know how to set import property ~s (type ~s?)"
                prop type)))))
