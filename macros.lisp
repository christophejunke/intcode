(in-package :advent.2019.intcode)

;;; PROCESSORS

(defmacro defproc (name supers slots)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,supers ,slots (:metaclass processor-class))))

;;; PRIMITIVES

;; TODO: make an expander generic function, and macrolet it when
;; expanding instructions.
(defmacro define-special (name (processor &rest args) &body body)
  "Like defmacro, but register name in processor class"
  (destructuring-bind (processor &optional (class 'processor))
      (ensure-list processor)
    `(progn
       (defmacro ,name (,processor ,@args)
	 (declare (ignorable ,processor))
	 ,@body)
       (register-primitive (find-class ',class) ',name))))

(defmacro define-primitive (name (processor &rest args) &body body)
  (destructuring-bind (processor &optional (class 'processor))
      (ensure-list processor)
    (assert (char= (char (string name) 0) #\.))
    (with-gensyms (pclass primitives)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (let ((,pclass (find-class ',class)))
	   (let ((,primitives (processor-primitives ,pclass)))
	     (unless (find ',name ,primitives)
	       (vector-push-extend ',name
				   ,primitives
				   (max 1 (length ,primitives))))
	     (unless (fboundp ',name)
	       (defgeneric ,name (,processor ,@args)))
	     (defmethod ,name ((,processor ,class) ,@args)
	       ,@body)))))))

(defun delete-primitive (name class)
  (with-accessors ((p processor-primitives)) class
    (when-let (i (position name p))
      (prog1 t
	(let ((entry (shiftf (aref p i) (aref p (1- (length p))))))
	  (decf (fill-pointer p))
	  (fmakunbound entry))))))

(defmacro define-op ((class% opcode nick
			    &key (flow :next) store)
			      (processor &rest args)
		     code)
  (with-gensyms ($options _)
    (let* ((class (find-class class%))
	   (prototype (make-instance class))
	   (instruction (make-instruction prototype
					  opcode
					  nick
					  flow
					  (cons processor args)
					  store
					  code))
	   (body (expand-execute prototype
				 processor
				 $options
				 instruction)))
      `(progn
	 (setf (gethash ,opcode (processor-instructions ,class))
	       (make-instruction (make-instance ',class%)
				 ,opcode
				 ',nick
				 ,flow
				 '(,processor ,@args)
				 ',store
				 ',code))
	 (defmethod execute ((,processor ,class)
			     (,_ (eql ,opcode))
			     ,$options)
	   ,@body)))))

(defmacro run (class mem)
  `(run-program (make-instance ',class :memory (make-memory ,mem))))
