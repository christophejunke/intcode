(in-package :advent.2019.intcode)

;;; PATHS

(defvar *root* *default-pathname-defaults*)

(defun memory-pathname (name)
  (merge-pathnames name (make-pathname :type "in" :defaults *root*)))


