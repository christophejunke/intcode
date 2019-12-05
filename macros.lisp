(in-package :advent.2019.intcode)

;;; PRIMITIVES

(defmacro define-primitive (name (processor &rest args) &body body)
  (destructuring-bind (processor &optional (class 'processor))
      (ensure-list processor)
    (with-gensyms (pclass primitives)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (let ((,pclass (find-class ',class)))
	   (let ((,primitives (processor-primitives ,pclass)))
	     (unless (find ',name ,primitives)
	       (vector-push-extend ',name
				   ,primitives
				   (max 1 (length ,primitives))))
	     (defun ,name (,processor ,@args)
	       ,@body)))))))

(defun delete-primitive (name class)
  (with-accessors ((p processor-primitives)) class
    (when-let (i (position name p))
      (prog1 t
	(let ((entry (shiftf (aref p i) (aref p (1- (length p))))))
	  (decf (fill-pointer p))
	  (fmakunbound entry))))))

(defvar *modes* 0)

(defmacro define-op ((class opcode &key (next :auto)) (processor &rest args)
		     &body code)
  (with-gensyms (_ expander closure buffer-var pc-var body)
    (declare (ignore expander body closure))
    (loop
       with mask = 0
       for index from 0
       for a in args
       for immediate = (consp a)
       collect (if (consp a) (car a) a) into new-args
       do (setf (logbitp index mask) immediate)
       finally 
	 (return
	   (let ((args new-args)
		 (offset (1+ (length args))))
	     (let ((compile-time-expander
		    (eval `(lambda (,processor ,@args)
			     ,@code))))
	       `(defmethod execute ((,processor ,class)
				    (,_ (eql ,opcode)))
		  (let ((,buffer-var (buffer (memory ,processor)))
			(,pc-var (pc ,processor)))
		    (declare (ignorable ,buffer-var ,pc-var))
		    (flet ((.next () (+ ,pc-var ,offset)))
		      (multiple-value-bind (,@args)
			  (unpack ,processor
				  ,opcode
				  (logior *modes* ,mask)
				  ,@(loop
				       for o from 1
				       for a in args
				       collect `(aref ,buffer-var 
						      (+ ,pc-var ,o))))
			,(apply compile-time-expander processor args))
		      ,@(when next
			  `((setf (pc ,processor)
				  ,(typecase next
				     ((eql :auto) `(.next))
				     ((integer 1) `(+ ,pc-var ,next))
				     (cons (destructuring-bind (addr) next
					     (check-type addr (integer 0))
					     addr)))))))))))))))
