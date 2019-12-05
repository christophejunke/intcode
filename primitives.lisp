(in-package :advent.2019.intcode)

;;;; PRIMITIVES

(define-primitive .load (processor address)
  (at (memory processor) address))

(define-primitive .store (processor address value)
  (setf (at (memory processor) address) value))

(defmethod run-program :around ((p processor))
  (catch :halt (call-next-method)))

(define-primitive .halt (processor)
  (throw :halt processor))
