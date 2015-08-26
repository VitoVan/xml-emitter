(in-package :common-lisp)

(defpackage :xml-emitter
  (:use :cl :cl-utilities)
  (:export #:xml-out
	   #:xml-as-is
	   #:with-tag
	   #:with-simple-tag
	   #:with-xml-output
	   #:simple-tag
	   #:emit-simple-tags
	   ;; RSS 2.0
	   #:rss-channel-header
	   #:rss-item
	   #:with-rss2))