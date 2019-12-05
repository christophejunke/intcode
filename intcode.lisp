(in-package :advent.2019.intcode)

;;; PATHS

(defvar *root* *default-pathname-defaults*)

(defun memory-pathname (name)
  (merge-pathnames name (make-pathname :type "in" :defaults *root*)))

;;;; PROCESSOR

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

;;; MEMORY

(defclass memory ()
  ((%buffer :initarg :buffer :reader buffer)))

(deftype opcode () 'fixnum)

(defgeneric make-memory (in)
  (:method ((buffer vector))
    (make-instance 'memory :buffer buffer))
  (:method ((line string))
    (make-memory (map '(vector opcode) #'parse-integer (split #\, line))))
  (:method ((path pathname))
    (with-open-file (in (memory-pathname path))
      (make-memory (read-line in))))
  (:method ((original memory))
    (make-instance (class-of original)
		   :buffer (copy-seq (buffer original)))))

(defgeneric at (place address)
  (:method ((buffer vector) (index integer))
    (aref buffer index))
  (:method ((memory memory) address)
    (at (buffer memory) address))
  (:method ((p processor) relative)
    (at (memory p) (+ (pc p) relative))))

(defgeneric (setf at) (value place address)
  (:method (value (buffer vector) (index integer))
    (setf (aref buffer index) value))
  (:method (value (memory memory) address)
    (setf (at (buffer memory) address) value))
  (:method (value (p processor) relative)
    (setf (at (memory p) (+ (pc p) relative)) value)))

;;; INSTRUCTIONS

(defclass instruction ()
  ((opcode :reader opcode :initarg :opcode)
   (control-flow :reader control-flow :initarg :control-flow :initform :next)
   (args :reader args :initarg :args)
   (store :reader store :initarg :store)
   (expander :reader expander :initarg :expander)
   (mode-mask :reader mode-mask :initform 0 :initarg :mode-mask)
   (nick :reader nick :initarg :nick :initform "?")))

(defmethod print-object ((o instruction) stream)
  (print-unreadable-object (o stream :type t :identity nil)
    (format stream "~a/~d [~2,'0d]" (nick o) (length (args o)) (opcode o))))

(defstruct (arg (:conc-name arg)) name kind)

(defun parse-argument (arg)
  (destructuring-bind (arg &optional kind) (ensure-list arg)
    (make-arg :name arg
	      :kind (ecase kind
		      ((:addr nil) kind)))))

(defgeneric argnames (args)
  (:method ((args sequence))
    (map 'list #'argname args))
  (:method ((inst instruction))
    (argnames (args inst))))

(defun make-instruction (opcode nickname flow arguments store expr)
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
       for a$ = (parse-argument a)
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
				     ``((setf (at (memory ,,processor)
						  ,,store)
					      ,,expr))
				     ``(,,expr))))
		  :mode-mask mask
		  :nick nickname)))))

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

;;; META

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
