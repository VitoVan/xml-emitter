;; -*- Lisp -*-

(defpackage #:xml-emitter-system
  (:use #:common-lisp #:asdf))

(in-package #:xml-emitter-system)

(defsystem xml-emitter
  :author "Vito Van / Peter Scott"
  :licence "Public Domain / 0-clause MIT"
  :serial t
  :components ((:static-file "LICENCE")
               (:file "package")
               (:file "xml")
               (:file "rss2"))
  :depends-on (cl-utilities)
  :description "xml-emitter simply emits XML, with some
complexity for handling indentation. It can be used to produce all
sorts of useful XML output; it has an RSS 2.0 emitter built in.")
