(in-package :advent.2019.intcode)

(defvar *amplifier-controller* (make-memory #P"07.in"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:queues :queues.simple-queue))
  (use-package :queues)
  (defclass v3/d7 (v3) 
    ((name :accessor name :initarg :name)
     (in :accessor in :initarg :in)
     (out :accessor out :initarg :out))
    (:metaclass processor-class)))

(define-primitive .print ((p v3/d7) value)
  (funcall (out p) value))

(define-primitive .read ((p v3/d7))
  (qpop (in p) :empty))

(defun make-amplifiers (phases callback
			&optional (program *amplifier-controller*))
  (nreverse
   (loop
      for p across (reverse phases)
      for i from (length phases) downto 0
      for next = nil then amplifier
      for amplifier = (make-instance 'v3/d7
				     :name (format nil "AMPLIFIER-~d" i)
				     :memory (make-memory program)
				     :in (make-queue :simple-queue)
				     :out (if next
					      (let ((n next)) ;; << rebind!
						(lambda (v) (qpush (in n) v)))
					      callback))
      collect amplifier
      do (qpush (in amplifier) p)
	(when (= i 1)
	  (qpush (in amplifier) 0)))))

(defmethod print-object ((o v3/d7) stream)
  (print-unreadable-object (o stream)
    (format stream "[~a] in: ~a" (name o) (in o))))

(defun test ()
  (make-amplifiers #(4 3 2 1 0)
		   #'print
		   "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))

(let ((max 0))
  (flet ((maximizer (value)
	   (when (> value max)
	     (setf max value))))
    (map-permutations (lambda (phases)
			(handler-case
			    (let ((chain (make-amplifiers phases #'maximizer)))
			      (map () #'run-program chain))
			  (error (e)
			    (warn "phase ~a: error ~s" phases e))))
		      #(0 1 2 3 4)))
  max)

=> 65464

;; day 7 - part 2

(define-primitive .read ((p v3/d7))
  (or (qpop (in p))
      (throw :halt p)))

(let ((max 0))
  (flet ((maximizer (value)
	   (when (> value max)
	     (setf max value))))
    (map-permutations 
     (lambda (phases)
       (handler-case
	   (let (chain)
	     (flet ((end-of-chain (value)
		      (maximizer value)
		      (qpush (in (first chain)) value)))
	       (setf chain (make-amplifiers phases 
					    #'end-of-chain))
	       (loop
		  for prev = (mapcar #'pc chain) then pcs
		  for pcs = (map 'list 
				 (lambda (p) 
				   (run-program p)
				   (pc p))
				 chain)
		  until (equalp pcs prev))))
	 (error (e)
	   (warn "phase ~a: error ~s" phases e))))
     #(5 6 7 8 9)))
  max)


