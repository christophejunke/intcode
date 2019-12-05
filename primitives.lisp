(in-package :advent.2019.intcode)

;;;; PRIMITIVES

(define-primitive .load (processor address)
  (aref (buffer (memory processor)) address))

(define-primitive .store (processor address value)
  (setf (aref (buffer (memory processor)) address) value))

(defmethod run-program :around ((p processor))
  (catch :halt (call-next-method)))

(define-primitive .halt (processor)
  (throw :halt processor))

