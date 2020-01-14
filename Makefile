.PHONY: test test-sbcl test-ros

lisps := $(shell find .  -type f \( -iname \*.asd -o -iname \*.lisp \))

cl-print-version-args := --eval '\
	(progn \
		(print (lisp-implementation-version)) \
		(terpri))'

cl-test-args := --eval '\
	(progn \
		(ql:quickload :xml-emitter/tests :verbose T) \
		(let ((exit-code 0)) \
			(handler-case (asdf:test-system :xml-emitter) \
				(error (c) \
					(format T "~&~A~%" c) \
					(setf exit-code 1))) \
			(uiop:quit exit-code)))'

all: test

# Tests -----------------------------------------------------------------------

test: test-sbcl

test-sbcl: $(lisps)
	sbcl --noinform $(cl-test-args)

test-ros: $(lisps)
	ros run $(cl-print-version-args) $(cl-test-args)
