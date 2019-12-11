(in-package :advent.2019.intcode)

;; day 11 - part 1

(defvar *paint-program* (make-memory '(#P"11.in" 10000)))

(defproc v4/d11 (v4)
  ((pos :accessor pos :initform 0)
   (angle :accessor angle :initform #C(0 1))
   (commands :initform (make-array 2 :fill-pointer 0) :accessor commands)
   (grid :accessor grid :initform (make-hash-table))))

(defun update (proc color rotation)
  (with-accessors ((p pos) (g grid) (a angle)) proc
    (setf (gethash p g) color)
    (setf a (* a (ecase rotation
		   (0 #C(0 1))
		   (1 #C(0 -1)))))
    (incf p a)))

(define-primitive .read ((p v4/d11))
  (gethash (pos p) (grid p) 0))

(define-primitive .print ((p v4/d11) v)
  (let ((com (commands p)))
    (when (= 1 (vector-push v com))
      (let ((color (aref com 0))
	    (rotation (aref com 1)))
	(setf (fill-pointer com) 0)
	(update p color rotation)))))

(let ((p (make-instance
	  'v4/d11 
	  :memory (make-memory *paint-program*))))
  (run-program p)
  (hash-table-count (grid p)))

;; reuse code from day 08

(ql:quickload :zpng)
(defun draw-image (file 2d &optional (scale 10))
  (destructuring-bind (rows cols) (array-dimensions 2d)
    (let ((png (make-instance 'zpng:pixel-streamed-png
			      :color-type :grayscale
			      :width (* cols scale)
			      :height (* rows scale))))
      (with-open-file (out file
			   :direction :output
			   :if-exists :supersede 
			   :element-type '(unsigned-byte 8))
	(prog1 (pathname out)
	  (zpng:start-png png out)
	  (dotimes (r rows)
	    (dotimes (_ scale)
	      (dotimes (c cols)
		(let ((color (case (aref 2d r c)
			       (0 '(0))
			       (1 '(255)))))
		  (dotimes (% scale)
		    (zpng:write-pixel color png))))))
	  (zpng:finish-png png))))))


;; day 11 - part 2

(defun rasterize (whites lx hx ly hy)
  (let ((origin (complex lx hy))
	(array (make-array (list (1+ (- hy ly))
				 (1+ (- hx lx)))
			   :element-type '(integer 0 1)
			   :initial-element 0)))
    (prog1 array
      (dolist (point whites)
	(let ((p (- point origin)))
	  (setf (aref array (- (imagpart p)) (realpart p)) 1))))))

(let ((p (make-instance
	  'v4/d11 
	  :memory (make-memory *paint-program*))))
  (setf (gethash 0 (grid p)) 1)
  (run-program p)
  (let ((whites (mapcar #'car (delete 0
				      (hash-table-alist (grid p))
				      :key #'cdr))))
    (loop 
       for w in whites
       for r = (realpart w)
       for i = (imagpart w)
       maximize r into hi-x
       maximize i into hi-y
       minimize r into lo-x
       minimize i into lo-y
       finally (return
		 (draw-image #P"11.png" 
			     (rasterize whites lo-x hi-x lo-y hi-y))))))

