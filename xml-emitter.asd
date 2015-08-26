;; -*- Lisp -*-

(defpackage #:xml-emitter-system
  (:use #:common-lisp #:asdf))

(in-package #:xml-emitter-system)

(defsystem xml-emitter
    :author "Peter Scott"
    :serial t
    :components ((:file "package")
		 (:file "xml")
		 (:file "rss2"))
    :depends-on (cl-utilities))