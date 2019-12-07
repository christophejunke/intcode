(in-package :advent.2019.intcode)

(defstruct node links)

;; example
(let ((v3 (make-instance 'v3 :memory (make-memory #P"05.in"))))
  (multiple-value-bind (instruction options) (fetch v3)
    (let ((processor (copy-symbol :p)))
      (apply (expander instruction)
	     processor
	     (argnames (args instruction))))))

;; => ((.STORE #:P ADDR (.READ #:P)))
