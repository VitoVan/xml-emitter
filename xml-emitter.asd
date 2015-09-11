;; -*- Lisp -*-

(defpackage #:xml-emitter-system
  (:use #:common-lisp #:asdf))

(in-package #:xml-emitter-system)

(defsystem xml-emitter
  :author "Vito Van / Peter Scott"
  :license "GPL v2"
  :serial t
  :components ((:file "package")
               (:file "xml")
               (:file "rss2"))
  :depends-on (cl-utilities))
