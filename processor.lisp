(in-package :advent.2019.intcode)

(defclass processor-class (c2mop:standard-class)
  ((%instructions
    :accessor processor-instructions
    :initform (make-hash-table))
   (%primitives
    :accessor processor-primitives
    :initform (make-array 10 :adjustable t :fill-pointer 0))))

(defmethod c2mop:validate-superclass ((c processor-class) (s standard-class))
  t)

(defclass processor ()
  ((counter :initform 0 :accessor pc)
   (memory :reader memory :initarg :memory))
  (:metaclass processor-class))

(defgeneric decode (processor value)
  (:method ((p processor) opcode) opcode))

(defgeneric run-program (processor)
  (:method ((p processor))
    (loop
       for value = (at (memory p) (pc p))
       do (multiple-value-bind (opcode options) (decode p value)
	    (execute p opcode options)))))

(defgeneric expand-execute (processor-prototype $proc $options instruction
			    &optional $jump))

(defgeneric primitives (thing)
  (:method-combination nconc)
  (:method nconc ((s symbol))
	   (primitives (find-class s)))
  (:method nconc (_)
	   (list))
  (:method nconc ((c processor-class))
	   (delete-duplicates
	    (nconc (coerce (processor-primitives c) 'list)
		   (mapcan #'primitives
			   (c2mop:class-direct-superclasses c)))))
  (:method nconc ((p processor))
	   (primitives (class-of p))))

(defgeneric instructions (thing)
  (:method-combination nconc)
  (:method nconc ((s symbol))
	   (instructions (find-class s)))
  (:method nconc (_)
	   (list))
  (:method nconc ((c processor-class))
	   (delete-duplicates
	    (nconc (hash-table-values (processor-instructions c))
		   (mapcan #'instructions
			   (c2mop:class-direct-superclasses c)))))
  (:method nconc ((p processor))
	   (instructions (class-of p))))

(defun fetch (processor)
  "Return current instruction pointed by processor, and decoded options"
  (multiple-value-bind (opcode options)
      (decode processor
	      (at (memory processor)
		  (pc processor)))
    (values (or (find opcode (instructions processor) :key #'opcode)
		(error "Instruction not found: ~a (processor: ~s)"
		       opcode processor))
	    options)))
