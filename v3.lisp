(in-package :advent.2019.intcode)

;; day 5 - part 2

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass v3 (v2) ()
    (:metaclass processor-class)))

(define-primitive ..when ((p v3) test expr)
  (declare (ignore p))
  `(if ,test ,expr))

(define-primitive ..ite ((p v3) test then else)
  (declare (ignore p))
  `(if ,test ,then ,else))

(define-op (v3 5 :jit :flow :jump) (p test label)
  `(..when ,p (not (= ,test 0)) (:jump ,label)))

(define-op (v3 6 :jif :flow :jump) (p test label)
  `(..when ,p (= ,test 0) (:jump ,label)))

(define-op (v3 7 :lt :store res) (p v1 v2 res)
  `(..ite ,p (< ,v1 ,v2) 1 0))

(define-op (v3 8 :eq :store res) (p v1 v2 res)
  `(..ite ,p (= ,v1 ,v2) 1 0))

(defun test-v3 ()
  (run v3 #P"05.in"))
