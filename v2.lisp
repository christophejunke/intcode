(in-package :advent.2019.intcode)

(defclass v2 (v1) ()
  (:metaclass processor-class))

(defmethod run-program ((p v2))
  (loop 
     for opcode = (aref (buffer (memory p)) (pc p))
     do
       (multiple-value-bind (m op) (truncate opcode 100)
	 (let ((*modes* (parse-integer (format () "~d" m) :radix 2)))
	   (execute p op)))))

(defmethod unpack ((p v2) opcode modes &rest args)
  (values-list
   (loop
      with mem = (buffer (memory p))
      for a in args
      for o from 0
      collect (if (logbitp o modes) a (aref mem a)))))

(define-op (v2 1) (p x y (res))
  `(.store ,p ,res (+ ,x ,y)))

(define-op (v2 2) (p x y (res))
  `(.store ,p ,res (* ,x ,y)))

;; new primitives

(define-primitive .print ((p v2) value)
  (declare (ignore p))
  (format t "~&:: ~a~%" value)
  (finish-output))

(define-primitive .read ((p v2))
  (declare (ignore p))
  (loop
     (format *query-io* "~&Enter value: ")
     (finish-output *query-io*)
     (if-let (v (parse-integer (read-line *query-io*) :junk-allowed t))
       (return v))))

;; new opcodes

(define-op (v2 3) (p (addr))
  `(.store ,p ,addr (.read ,p)))

(define-op (v2 4) (p (addr))
  `(.print ,p (.load ,p ,addr)))

(run-program
 (make-instance 'v2 :memory (make-memory "3,0,4,0,99")))

;;
;; INTCODE> (run-program (make-instance 'v2 :memory (make-memory "3,0,4,0,99")))
;;
;; Enter value: 40
;;
;; :: 40
;; #<V2 {100339FDE3}>

(defun v2 (mem)
  (run-program (make-instance 'v2 :memory (make-memory mem))))

;; (buffer (memory (v2 "1002,4,3,4,33")))
;; #(1002 4 3 4 99)

(v2 #P"05.in")

(define-primitive .jump-if (p test then else)
  (setf (pc p) (if test then else)))

(define-op (v2 5 :next nil) (p test label)
  `(.jump-if ,p (not (zerop ,test)) ,label (.next)))

(define-op (v2 6 :next nil) (p test label)
  `(.jump-if ,p (zerop ,test) ,label (.next)))

(define-op (v2 7) (p v1 v2 (addr))
  `(.store ,p ,addr (if (< ,v1 ,v2) 1 0)))

(define-op (v2 8) (p v1 v2 (addr))
  `(.store ,p ,addr (if (= ,v1 ,v2) 1 0)))

(v2 #P"05.in")
