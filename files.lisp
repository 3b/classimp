(in-package #:classimp-sample)

(defparameter *file-list* nil)
(defparameter *file-index* 0)

;; fixme: figure out actual supported extension list...
(defparameter *extensions*
  (ai:get-extension-list)
  #++'("x" "X" "WRL" "ter" "max" "STL" "smd" "raw" "q3s" "q3o" "ply"
    "off" "mtl" "obj" "mat" "nff" "mdl"
    "md5mesh"
    "md3" "md2"
    "lxo" "lwo" "irrmesh" "irr" "hmp" "dxf"
    "dae" "xml"
    "csm" "bvh" "b3d" "ase" "ac"
    "3ds"
    "3d" "uc"))

(defparameter *ignore-path-components* '("invalid"))

(defparameter *ignore-files* '("tga" "svn-base"  "jpg" "txt"  "png" "bmp"
                               "blend" "shader"  "lws" "rtf"
                               "md5anim""material" nil))

(defun find-files (path)
  (let ((foo nil)
        (unknown nil))
    (fad:walk-directory
     path (lambda (a) (push a foo))
     :test (lambda (f)
             (unless (member (pathname-type f)
                             *extensions* :test 'equalp)
               (unless (member (pathname-type f)
                               *ignore-files* :test 'equalp)
                 (pushnew (pathname-type f)
                          unknown :test 'equalp)))
             (unless (or (member (pathname-type f)
                                 *ignore-files* :test 'equalp)
                         (loop
                           for p in (pathname-directory
                                     (enough-namestring
                                      f path))
                             thereis (member p *ignore-path-components*
                                             :test 'equalp)))
               (member (pathname-type f)
                       *extensions* :test 'equalp))))
    (when foo (setf *file-list* foo))
    (format t "~&found ~s~%" (length foo))
    (format t "unknown extensions ~s~%" unknown)))

(defun next-file-key (w &optional (delta 1))
  (when *file-list*
    (setf *file-index* (mod (+ *file-index* (+ delta (length *file-list*)))
                            (length *file-list*)))
    (when (minusp *file-index*) (setf *file-index* 0))
    (setf *filename* (elt *file-list* *file-index*))
    (format t "load next file (~s) = ~s~%" *file-index* *filename*)
    (reload-scene w)
    (format t "~a/~a~%" *file-index* (length *file-list*))))


;(find-files (merge-pathnames "src/assimp/test/models" (user-homedir-pathname)))
