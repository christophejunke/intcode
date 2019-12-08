(in-package :advent.2019.intcode)

;; day 5 - part 1

(defproc v2 (v1) ())

(defun modebits (modes)
  (do ((bits 0) (i 0 (1+ i)))
      ((zerop modes) bits)
    (multiple-value-bind (%m %d) (truncate modes 10)
      (setf (logbitp i bits) (= 1 %d))
      (setf modes %m))))

(defmethod decode ((p v2) value)
  (multiple-value-bind (modes opcode) (truncate value 100)
    (values opcode `(:modes ,(modebits modes)))))

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

(define-op (v2 03 :in :store addr) (p addr)
  `(.read ,p))

(define-op (v2 04 :out) (p (a :addr))
  `(.print ,p (.load ,p ,a)))

(equalp (buffer (memory (run v2 "1002,4,3,4,33")))
	#(1002 4 3 4 99))

(defun test-v2 ()
  (run v2 #P"05.in"))
