(defsystem :intcode
  :depends-on (:alexandria :cl-ppcre :closer-mop)
  :components ((:file "package")
	       (:file "macros")
	       (:file "intcode")
	       (:file "v1")))
