(in-package :advent.2019.intcode)

(defvar *amplifier-controller* (make-memory #P"07.in"))

(defproc v3/d7 (v3)
  ((name :accessor name :initarg :name)
   (in :accessor in :initarg :in)
   (out :accessor out :initarg :out)))

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

(assert
 (= 65464
    (let ((max 0))
      (labels ((keep-max (value)
		 (when (> value max) (setf max value)))
	       (test (phases)
		 (mapc #'run-program (make-amplifiers phases #'keep-max))))
	(map-permutations #'test #(0 1 2 3 4)))
      max)))

;; day 7 - part 2

(define-primitive .read ((p v3/d7))
  (or (qpop (in p))
      (.halt p)))

(assert
 (= 1518124
    (let ((max 0))
      (flet ((run/pc (p) (run-program p) (pc p))
	     (keep-max (value)
	       (prog1 value
		 (when (> value max)
		   (setf max value)))))
	(map-permutations
	 (lambda (phases &aux chain)
	   (flet ((end-of-chain (v) (qpush (in (first chain)) (keep-max v))))
	     (setf chain (make-amplifiers phases #'end-of-chain))
	     (loop
		for pre = (mapcar #'pc chain) then pcs
		for pcs = (mapcar #'run/pc chain)
		until (equalp pcs pre))))
	 #(5 6 7 8 9)))
      max)))
