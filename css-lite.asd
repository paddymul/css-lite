;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem :css-lite
  :depends-on ("parenscript")
  :serial t
  :version "0.01"
  :components ((:file "package")
               (:file "css-lite")
               (:file "utility")
               (:file "lite-utility")
               (:file "paren-css-lite")))

(asdf:defsystem :css-lite/test
  :serial t
  :depends-on ("fiveam" "css-lite")
  :pathname "test/"
  :components ((:file "package")
               (:file "css-lite"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :css-lite)))
