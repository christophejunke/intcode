(defsystem :intcode
  :depends-on (:alexandria :cl-ppcre :closer-mop)
  :components ((:file "package")
	       (:file "intcode")
	       (:file "macros")	       
	       (:file "primitives")
	       (:file "v1")))
