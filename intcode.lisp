(in-package :advent.2019.intcode)

;;; PATHS

(defvar *root* *default-pathname-defaults*)

(defun memory-pathname (name)
  (merge-pathnames name (make-pathname :type "in" :defaults *root*)))

;;; MEMORY

(defclass memory ()
  ((%buffer :initarg :buffer :reader buffer)))

(deftype opcode () '(unsigned-byte 32))

(defgeneric make-memory (in)
  (:method ((buffer vector))
    (make-instance 'memory :buffer buffer))
  (:method ((line string))
    (make-memory (map '(vector opcode) #'parse-integer (split #\, line))))
  (:method ((path pathname))
    (with-open-file (in (memory-pathname path))
      (make-memory (read-line in))))
  (:method ((original memory))
    (make-instance (class-of original)
		   :buffer (copy-seq (buffer original)))))

;;; PROCESSOR

(defgeneric execute (processor opcode))

(defgeneric run-program (processor)
  (:method (processor)
    (loop (execute processor 
		   (aref (buffer (memory processor)) 
			 (pc processor))))))

;;; INSTRUCTIONS

(defclass processor-class (c2mop:standard-class)
  ((%primitives 
    :accessor processor-primitives
    :initform (make-array 10 :adjustable t :fill-pointer 0))))

(defmethod c2mop:validate-superclass ((c processor-class) (s standard-class))
  t)

(defclass processor ()
  ((counter :initform 0 :accessor pc)
   (memory :reader memory :initarg :memory))
  (:metaclass processor-class))

(defgeneric primitives (thing)
  (:method ((c processor-class))
    (copy-seq (processor-primitives c)))
  (:method ((p processor))
    (primitives (class-of p))))

;;;; BASE IMPLEMENTATION

(define-primitive .load (processor address)
  (aref (buffer (memory processor)) address))

(define-primitive .store (processor address value)
  (setf (aref (buffer (memory processor)) address) value))

(defmethod run-program :around ((p processor))
  (catch :halt (call-next-method)))

(define-primitive .halt (processor)
  (throw :halt processor))

