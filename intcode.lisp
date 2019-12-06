(in-package :advent.2019.intcode)

;;; PATHS

(defvar *root* *default-pathname-defaults*)

(defun memory-pathname (name)
  (merge-pathnames name (make-pathname :type "in" :defaults *root*)))

;;; INSTRUCTIONS

(defmethod expand-execute ((processor-prototype processor)
			   $proc
			   $options
			   instruction
			   &optional ($jump :jump))
  (with-gensyms ($buffer $pc $decoded $next $target $modes)
    (let ((default-jump (1+ (length (args instruction))))
	  (body
	   (loop
	      for i from 1
	      for a across (args instruction)
	      for n = (argname a)
	      for get-arg = `(aref ,$buffer (+ ,$pc ,i))
	      for e = (if (logbitp (1- i) (mode-mask instruction))
			  get-arg
			  `(let ((,$decoded ,get-arg))
			     (declare (fixnum ,$decoded))
			     (if (logbitp ,(1- i) ,$modes)
				 ,$decoded
				 (aref ,$buffer ,$decoded))))
	      collect n into names
	      collect (list n e) into bindings
	      finally (return
			(let ((inner (apply (expander instruction)
					    $proc
					    names)))
			  (if bindings
			      `((let ,bindings ,@inner))
			      inner))))))
      `((destructuring-bind (&key ((:modes ,$modes) 0)) ,$options
	  (let ((,$modes (logior ,$modes ,(mode-mask instruction)))
		(,$buffer (buffer (memory ,$proc)))
		(,$pc (pc ,$proc)))
	    (declare (ignorable ,$modes ,$buffer ,$pc))
	    ,@(ecase (control-flow instruction)
		(:stop body)
		(:next `(,@body (incf (pc ,$proc) ,default-jump)))
		(:jump `((let ((,$next))
			   (flet ((,$jump (,$target) (setf ,$next ,$target)))
			     (declare (inline ,$jump))
			     ,@body
			     (setf (pc ,$proc)
				   (or ,$next
				       (+ ,$pc ,default-jump))))))))))))))


