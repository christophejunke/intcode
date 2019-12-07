(in-package :advent.2019.intcode)

;;; PATHS

(defvar *root* (truename #P"~/data/advent/2019/"))

(defun memory-pathname (name)
  (merge-pathnames name (make-pathname :type "in" :defaults *root*)))


