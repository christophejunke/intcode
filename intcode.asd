(defsystem :intcode
  :depends-on (:alexandria :cl-ppcre :closer-mop)
  :components ((:file "package")
	       (:file "memory")
	       (:file "instructions")
	       (:file "processor")
	       (:file "intcode")
	       (:file "macros")	       
	       (:file "primitives")
	       (:file "base")
	       (:file "v1")
	       (:file "v2")
	       (:file "v3")))
