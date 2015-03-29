(cl:in-package :%open-asset-import-library)

(define-foreign-library assimp
  (:darwin "libassimp.dylib")
  (:windows "assimp.dll" );; :calling-convention :stdcall ?
  #++(:unix "libassimp.so")
  (:unix (:or "libassimp.so.3" "libassimp3.0.so" "libassimp.so")))

(use-foreign-library assimp)
