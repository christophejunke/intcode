(in-package :advent.2019.intcode)

;;;; INSTRUCTIONS

(defclass v1 (processor) ()
  (:metaclass processor-class))

(define-op (v1 99 :next nil) (p)
  `(.halt ,p))

(define-op (v1 1) (p x y res)
  `(.store ,p ,res (+ (.load ,p ,x)
		      (.load ,p ,y))))

(define-op (v1 2) (p x y res)
  `(.store ,p ,res (* (.load ,p ,x)
		      (.load ,p ,y))))

;;; REPLAYING DAY 02

(defun test-day-02-part-1 (&optional (in #P"02.in"))
  (let ((p (make-instance 'v1 :memory (make-memory in))))
    (.store p 1 12)
    (.store p 2 2)
    (run-program p)
    (assert (= 3058646 (.load p 0)))))
