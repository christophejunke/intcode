(in-package :advent.2019.intcode)

(defmethod make-memory ((in cons))
  (destructuring-bind (in size) in
    (let ((memory (make-memory in)))
      (assert (>= size (length (buffer memory))))
      (make-memory (adjust-array (buffer memory)
				 size
				 :initial-element 0)))))

;; (buffer (make-memory '("0,1,2" 10)))
;; #(0 1 2 0 0 0 0 0 0 0)

(defproc v4 (v3) 
  ((relative-base :initform 0 :accessor relative-base)))

(defun modes (modes size)
  (let ((result (make-array size :initial-element 0)))
    (prog1 result
      (do ((digits nil) (i 0 (1+ i)))
	  ((zerop modes) (replace result (nreverse digits)))
	(multiple-value-bind (%m %d) (truncate modes 10)
	  (push %d digits)
	  (setf modes %m))))))

;; (modes 0 4)
;; #(0 0 0 0)

;; (modes 200 4)
;; #(0 0 2 0)

;; (modes 1200 4)
;; #(0 0 2 1)

(defmethod decode ((p v4) opcode)
  (multiple-value-bind (modes opcode) (truncate opcode 100)
    (let ((size (length (args (find opcode (instructions p) :key #'opcode)))))
      (values opcode `(:modes ,(modes modes size))))))

(defmethod expand-execute ((processor-prototype v4)
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
	      for m across (mode-mask instruction) 
	      for n = (argname a)
	      for get-arg = `(aref ,$buffer (+ ,$pc ,i))
	      for e = (case m
			(1 `(let ((,$decoded ,get-arg))
			      (ecase (aref ,$modes ,(1- i))
				((0 1) ,$decoded)
				(2 (+ (relative-base ,$proc)
				      ,$decoded)))))
			(t
			 `(let ((,$decoded ,get-arg))
			    (ecase (aref ,$modes ,(1- i))
			      (0 (aref ,$buffer ,$decoded))
			      (1 ,$decoded)
			      (2 (aref ,$buffer (+ (relative-base ,$proc)
						   ,$decoded)))))))
	      collect n into names
	      collect (list n e) into bindings
	      finally (return
			(let ((inner (apply (expander instruction)
					    $proc
					    names)))
			  (if bindings
			      `((let ,bindings ,@inner))
			      inner))))))
      `((destructuring-bind (&key ((:modes ,$modes))) ,$options
	  (declare (ignorable ,$modes))
	  (check-type ,$modes (not null))
	  (let ((,$buffer (buffer (memory ,$proc)))
		(,$pc (pc ,$proc)))
	    (declare (ignorable ,$buffer ,$pc))
	    (macrolet ((.store (,$p ,$a ,$v)
			 (declare (ignorable ,$p))
			 `(setf (aref ,',$buffer ,,$a) ,,$v)))
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
    ((processor-prototype v4) opcode nickname flow %arguments store expr)
  (check-type store symbol)
  (destructuring-bind (processor . arguments) %arguments
    (loop
       with flow = (ecase flow
		     ((:stop 0 nil) :stop)
		     ((:next 1 t) :next)
		     ((:jump) flow))
       with mask = (make-array (length arguments) :initial-element 0)
       for index from 0
       for a in arguments
       for a$ = (parse-argument processor-prototype a)
       collect a$ into args$
       do (when (or (eq (argname a$) store)
		    (eq (argkind a$) :addr))
	    (setf (aref mask index) 1))
       finally (return
		 (make-instance
		  'instruction
		  :opcode opcode
		  :expr expr
		  :params %arguments
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

;; redefine existing instructions for v4
(let ((prototype (make-instance 'v4)))
  (dolist (instruction (instructions 'v4))
    (let ((instruction (make-instruction prototype
					 (opcode instruction)
					 (nick instruction)
					 (control-flow instruction)
					 (params instruction)
					 (store instruction)
					 (expression instruction))))
      (let ((opcode (opcode instruction)))
	(with-gensyms (_ $options $processor)
	  (eval
	   `(defmethod execute ((,$processor v4)
				(,_ (eql ,opcode))
				,$options)
	      (declare (optimize (debug 3)))
	      ,@(expand-execute prototype
				$processor
				$options
				instruction))))))))


(define-primitive .incbase ((p v4) delta)
  (incf (relative-base p) delta))

(define-op (v4 09 :base) (p d)
  `(.incbase ,p ,d))

;; fixed an old bug
(define-op (v4 04 :out) (p a)
  `(.print ,p ,a))

(run v4 '("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" 1000))

;; (instructions 'v4)

;; fixed an old bug
(define-op (v2 04 :mul :store res) (p a b res)
  `(* ,a ,b))

(run v4 '("1102,34915192,34915192,7,4,7,99,0" 1000))
(run v4 '("104,1125899906842624,99" 1000))

;; (run v4 '(#P"09.in" 2048))
;; (run v4 #P"05.in")

