;; -*- Lisp -*-

(defpackage #:xml-emitter-system
  (:use #:common-lisp #:asdf))

(in-package #:xml-emitter-system)

(defsystem :xml-emitter
  :author "Peter Scott / Vito Van / Philipp Marek / Kieran Grant / Matteo Landi"
  :version "1.1.0"
  :license "Public Domain / 0-clause MIT"
  :serial t
  :components ((:static-file "LICENSE")
               (:file "package")
               (:file "xml")
               (:file "rss2"))
  :depends-on (cl-utilities)
  :description "xml-emitter simply emits XML, with some
complexity for handling indentation. It can be used to produce all
sorts of useful XML output; it has an RSS 2.0 emitter built in."
  :in-order-to ((test-op (test-op :xml-emitter/tests))))

(defsystem :xml-emitter/tests
  :author "Peter Scott / Vito Van / Philipp Marek / Kieran Grant / Matteo Landi"
  :license "Public Domain / 0-clause MIT"
  :serial t
  :components ((:file "tests"))
  :depends-on (:xml-emitter :1am)
  :description "xml-emitter test suite"
  :perform (test-op (o c) (uiop:symbol-call :1am '#:run)))
