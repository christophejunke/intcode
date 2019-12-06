(in-package :advent.2019.intcode)

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

(defgeneric parse-argument (processor arg)
  (:method (processor arg)
    (destructuring-bind (arg &optional kind) (ensure-list arg)
      (make-arg :name arg
		:kind (ecase kind
			((:addr nil) kind))))))

(defgeneric argnames (args)
  (:method ((args sequence))
    (map 'list #'argname args))
  (:method ((inst instruction))
    (argnames (args inst))))

(defgeneric make-instruction 
    (processor-prototype opcode nickname flow arguments store expr)
  (:method     
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
				       ``((setf (at (memory ,,processor)
						    ,,store)
						,,expr))
				       ``(,,expr))))
		    :mode-mask mask
		    :nick nickname))))))
