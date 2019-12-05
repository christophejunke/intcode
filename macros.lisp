(in-package :advent.2019.intcode)

;;; PRIMITIVES

(defmacro define-primitive (name (processor &rest args) &body body)
  (destructuring-bind (processor &optional (class 'processor))
      (ensure-list processor)
    (assert (char= (char (string name) 0) #\.))
    (let ((macrop (char= (char (string name) 1) #\.)))
      (with-gensyms (pclass primitives)
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   (let ((,pclass (find-class ',class)))
	     (let ((,primitives (processor-primitives ,pclass)))
	       (unless (find ',name ,primitives)
		 (vector-push-extend ',name
				     ,primitives
				     (max 1 (length ,primitives))))
	       (,(if macrop 'defmacro 'defun)
		 ,name (,processor ,@args)
		 ,@body))))))))

(defun delete-primitive (name class)
  (with-accessors ((p processor-primitives)) class
    (when-let (i (position name p))
      (prog1 t
	(let ((entry (shiftf (aref p i) (aref p (1- (length p))))))
	  (decf (fill-pointer p))
	  (fmakunbound entry))))))

(defmacro define-op ((class opcode nick
			    &key (flow :next) store)
			      (processor &rest args)
		     code)
  (with-gensyms ($options _)
    (let* ((class (find-class class))
	   (instruction (make-instruction opcode
					  nick
					  flow
					  (cons processor args)
					  store
					  code))
	   (body (expand-execute (make-instance class)
				 processor
				 $options
				 instruction)))
      (prog1 `(defmethod execute ((,processor ,class)
				  (,_ (eql ,opcode))
				  ,$options)
		,@body)
	(setf (gethash opcode (processor-instructions class))
	      instruction)))))

(defmacro run (class mem)
  `(run-program (make-instance ',class :memory (make-memory ,mem))))
