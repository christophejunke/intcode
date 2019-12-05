(in-package :advent.2019.intcode)

;;; PATHS

(defvar *root* *default-pathname-defaults*)

(defun memory-pathname (name)
  (merge-pathnames name (make-pathname :type "in" :defaults *root*)))

;;; MEMORY

(defclass memory ()
  ((%buffer :initarg :buffer :reader buffer)))

(deftype opcode () 'fixnum)

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

(defgeneric unpack (processor opcode modes &rest args)
  (:method (p o m &rest a) (values-list a)))

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
  (:method-combination nconc)
  (:method nconc ((s symbol))
	   (primitives (find-class s)))
  (:method nconc (_)
	   (list))
  (:method nconc ((c processor-class))
	   (delete-duplicates
	    (nconc (coerce (processor-primitives c) 'list)
		   (mapcan #'primitives
			   (c2mop:class-direct-superclasses c)))))
  (:method nconc ((p processor))
	   (primitives (class-of p))))

