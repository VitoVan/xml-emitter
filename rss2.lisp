;; This is some code for a simple RSS emitter. It lets you easily
;; construct RSS feeds using a common subset of RSS. It has some
;; advantages and disadvantages; it's fast and efficient, but it
;; doesn't support any advanced features of RSS. If you need them,
;; though, it shouldn't be hard to hack this to do what you want.

(in-package :xml-emitter)

(defun rss-channel-header (title link &key description (language "en-us")
		       image image-title image-link)
  (emit-simple-tags :title title
		    :link link
		    :description description
		    :language language)
  (when image
    (with-tag ("image")
      (emit-simple-tags :title (or image-title title)
			:url image
			:link (or image-link link)))))

(defun rss-item (title &key link description author category
		 comments guid pubDate source)
  (with-tag ("item")
    (emit-simple-tags :title title
		      :link link
		      :description description
		      :author author
		      :category category
		      :comments comments
		      :guid guid
		      "pubDate" pubDate
		      :source source)))

(defmacro with-rss2 ((stream &key (encoding "ISO-8859-1")) &body body)
  `(with-xml-output (,stream :encoding ,encoding)
     (with-tag ("rss" '(("version" "2.0")))
       (with-tag ("channel")
	 ,@body))))

;; This sample RSS feed demonstrates how to use the RSS emitter. As
;; you can see, you can use a fairly decent subset of RSS with
;; relative ease.

#+nil
(with-rss2 (*standard-output*)
  (rss-channel-header "Peter's Blog" "http://peter.blogspot.com/"
		      :description "A place where I sometimes post stuff"
		      :image "myhead.jpg"
		      :image-title "My glorious visage")
  (rss-item "Breaking news!"
	    :link "http://google.com/"
	    :description "The biggest problem with the DO-ODD macro above is that it puts BODY
into LOOP. Code from the user of the macro should never be run in the
environment established by the LOOP macro. LOOP does a number of
things behind your back, and it's hard to disable them. For example,
what happens here?"
	    :author "Peter Scott"
	    :category "Lisp"
	    :pubDate "Sun, 29 Sep 2002 19:59:01 GMT")
  (rss-item "RSS emitter created"
	    :description "An RSS emitter has been released! Hahahahaha!"
	    :author "Peter Scott"
	    :link "http://gmail.google.com/"))