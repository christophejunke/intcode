(in-package :advent.2019.intcode)

(defpackage :advent.2019.intcode.asm
  (:use)
  (:nicknames "IC.ASM"))

(defvar *instruction-package* 
  (find-package :advent.2019.intcode.asm))

(defun expand-instruction-form (instruction
				&optional 
				  (package *instruction-package*)
				  (prefix #\%))
  (let ((name (intern (format () "~a~a" prefix (string (nick instruction)))
		      package)))
    `((export ',name ,package)
      (defun ,name ,(argnames (args instruction))
	
	))))

;; a list value means: immediate
;; (except when the opcode alread treats an operand specially)
(defun % (address) (list address))

(eval
 `(progn
    ,@(mapcan #'expand-instruction-form 
	      (instructions 'v3))))

(use-package :ic.asm)

(%in 0)
(%jit 0 (% 10))
(%lt )

