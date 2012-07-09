(cl:in-package :%open-asset-import-library)

(define-foreign-library assimp
  (:windows "assimp.dll" );; :calling-convention :stdcall ?
  #++(:unix "libassimp.so")
  (:unix "libassimp3.0.so"))

(use-foreign-library assimp)
