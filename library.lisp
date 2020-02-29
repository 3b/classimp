(cl:in-package :%open-asset-import-library)

(define-foreign-library assimp
  (:darwin "libassimp.dylib")
  (:windows (:or "assimp.dll"
                 "libassimp.dll"
                 ;; locally built assimp has lots of dll names
                 ;; depending on what version of VC it was compiled
                 ;; with :/ please send a pr or bug report if you see
                 ;; others
                 "assimp-vc150-mt.dll"
                 "assimp-vc141-mt.dll"
                 "assimp-vc140-mt.dll"
                 "assimp-vc120-mt.dll"
                 "assimp-vc110-mt.dll"
                 "assimp-vc100-mt.dll"
                 "assimp-vc90-mt.dll"
                 "assimp-vc80-mt.dll"
                 "assimp-vc70-mt.dll"
                 ;; or it might specify 32/64-bit
                 #+(or x86-64 64-bit)
                 "assimp64.dll"
                 #-(or x86-64 64-bit)
                 "assimp32.dll"
                 ));; :calling-convention :stdcall ?
  (:unix (:or "libassimp.so.5"
              "libassimp.so.4"
              "libassimp.so.3" "libassimp3.0.so"
              "libassimp.so")))

(use-foreign-library assimp)
