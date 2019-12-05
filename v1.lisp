(in-package :advent.2019.intcode)

;;;; INSTRUCTIONS

(defclass v1 (processor) ()
  (:metaclass processor-class))

(define-op (v1 99 :halt :flow :stop) (p)
  `(.halt ,p))

(define-op (v1 01 :add :store res) (p x y res)
  `(+ ,x ,y))

(define-op (v1 02 :mul :store res) (p x y res)
  `(* ,x ,y))

;;; REPLAYING DAY 02

(defun test-day-02-part-1 (&optional (in #P"02.in"))
  (let ((p (make-instance 'v1 :memory (make-memory in))))
    (.store p 1 12)
    (.store p 2 2)
    (run-program p)
    (assert (= 3058646 (.load p 0)))))
