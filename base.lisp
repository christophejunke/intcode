(in-package :advent.2019.intcode)

(defmethod expand-execute ((processor-prototype processor)
			   $proc
			   $options
			   instruction
			   &optional ($jump :jump))
  (with-gensyms ($buffer $pc $decoded $next $target $modes $p $a $v)
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
	    (macrolet ((.store (,$p ,$a ,$v) `(setf (aref ,',$buffer ,,$a) ,,$v)))
	      ,@(ecase (control-flow instruction)
		  (:stop body)
		  (:next `(,@body (incf (pc ,$proc) ,default-jump)))
		  (:jump `((let ((,$next))
			     (flet ((,$jump (,$target) (setf ,$next ,$target)))
			       (declare (inline ,$jump))
			       ,@body
			       (setf (pc ,$proc)
				     (or ,$next
					 (+ ,$pc ,default-jump)))))))))))))))

(defmethod make-instruction     
    (processor-prototype opcode nickname flow arguments store expr)
  (check-type store symbol)
  (destructuring-bind (processor . arguments) arguments
    (loop
       with flow = (ecase flow
		     ((:stop 0 nil) :stop)
		     ((:next 1 t) :next)
		     ((:jump) flow))
       with mask = 0
       for index from 0
       for a in arguments
       for a$ = (parse-argument processor-prototype a)
       collect a$ into args$
       do (setf (logbitp index mask)
		(or (eq (argname a$) store)
		    (eq (argkind a$) :addr)))
       finally (return
		 (make-instance
		  'instruction
		  :opcode opcode
		  :store store
		  :args (coerce args$ 'vector)
		  :control-flow flow
		  :expander (eval
			     `(lambda (,processor ,@(argnames args$))
				(declare (ignorable ,processor))
				,(if store
				     ``((.store ,,processor ,,store ,,expr))
				     ``(,,expr))))
		  :mode-mask mask
		  :nick nickname)))))
