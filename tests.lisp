(defpackage :xml-emitter/tests
  (:use :common-lisp :xml-emitter))

(in-package :xml-emitter/tests)

(defmacro xml-test (name stream expected &body body)
  "Utility macro to facilitate testing of the generated xml

  `name`, is the name of the test
  `stream`, is a stream we can use in our test to initialize xml-emitter
  macros/functions with; we expected most of the tests to look like:

    (with-output-to-string (s)
      (with-xml (s)
        ...))

    so instead of having all the tests implement this, we figured it would be
    best if the testing infrastructure taken care of this for us, automatically
  `expected`, is the expected generated xml
  `body`, is a list of forms used to interact with xml-emitter macros/functions

  PS. This macro also abstracts away the details of the underlaying testing
  framework, but that's mostly incidental, as the real driver was to let
  tests focus on generating and comparing xml"
  `(1am:test ,name
     (1am:is (string= ,expected
                      (with-output-to-string (,stream)
                        ,@body)))))

(xml-test with-rss/default-args s
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
<rss version=\"2.0\">
    <channel>

    </channel>
</rss>"
  (with-rss2 (s)))

(xml-test with-rss/override-default-args s
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss xmlns:atom=\"http://www.w3.org/2005/Atom\" version=\"2.0\">
    <channel>

    </channel>
</rss>"
  (with-rss2 (s :encoding "utf-8" :attrs '(("xmlns:atom" "http://www.w3.org/2005/Atom")
                                           ("version" "2.0")))))
