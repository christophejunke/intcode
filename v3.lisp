(in-package :advent.2019.intcode)

;; day 5 - part 2

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass v3 (v2) ()
    (:metaclass processor-class)))

(define-special ..if (p test then else)
  `(if ,test ,then ,else))

(define-op (v3 5 :jit :flow :jump) (p test label)
  `(..if ,p (not (= ,test 0)) (:jump ,label) nil))

(define-op (v3 6 :jif :flow :jump) (p test label)
  `(..if ,p (= ,test 0) (:jump ,label) nil))

(define-op (v3 7 :lt :store res) (p v1 v2 res)
  `(..if ,p (< ,v1 ,v2) 1 0))

(define-op (v3 8 :eq :store res) (p v1 v2 res)
  `(..if ,p (= ,v1 ,v2) 1 0))

(defun test-v3 ()
  (run v3 #P"05.in"))
