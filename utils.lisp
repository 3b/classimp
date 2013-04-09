(in-package :classimp)

(defmacro with-foreign-slots* ((vars ptr type) &body body)
  "Create local symbol macros for each var in VARS to reference
foreign slots in PTR of TYPE.  Similar to WITH-SLOTS."
  (let ((ptr-var (gensym "PTR")))
    `(let ((,ptr-var ,ptr))
       (symbol-macrolet
           ,(loop for var in vars
               collect (if (and (listp var) (eq (first var) :pointer))
                           `(,(second var) (foreign-slot-pointer 
                                            ,ptr-var ',type ',(second var)))
                           (if (listp var)
                               (error "Malformed var names must be:~%name~% -or- ~%(:pointer name)")
                               `(,var (foreign-slot-value 
                                       ,ptr-var ',type ',var)))))
         ,@body))))
