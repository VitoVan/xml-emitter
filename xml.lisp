(in-package :xml-emitter)

;; Character escaping
;;;;;;;;;;;;;;;;;;;;;

;; This code was adapted from XMLS, by Miles Egan. Thanks, Miles.

(defvar *entities*
  #(("lt;" #\<)
    ("gt;" #\>)
    ("amp;" #\&)
    ("apos;" #\')
    ("quot;" #\")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *whitespace* (remove-duplicates 
                        '(#\Newline #\Space #\Tab #\Return #\Linefeed))))

(defvar *char-escapes*
  (let ((table (make-array 256 :element-type 'string :initial-element "")))
    (declare (type vector *entities*))
    (loop for code from 0 to 255
	  for char = (code-char code)
	  for entity = (first (find char *entities*
				    :test #'char= :key #'second))
	  do (setf (svref table code)
		   (cond
		     (entity
		      (concatenate 'string "&" entity))
		     ((and (or (< code 32) (> code 126))
			   (not (= code 10))
			   (not (= code 9)))
		      (format nil "&#x~x;" code))
		     (t
		      (format nil "~x" char))))
	  finally (return table))
    table))

(defun write-escaped (string stream)
  "Writes string to stream with all character entities escaped."
  (coerce string 'simple-base-string)
  (loop for char across string
        for esc = (svref *char-escapes* (char-code char))
        do (write-sequence esc stream)))

;; Low-level XML output
;;;;;;;;;;;;;;;;;;;;;;;

(defvar *xml-output-stream* *standard-output*
  "The stream to write XML to")

(defvar *indent* 0
  "Number of spaces to indent each line of XML output")

(defun indent (&optional (spaces *indent*))
  "Indent a given number of spaces"
  (loop repeat spaces do (write-char #\Space *xml-output-stream*)))

(defmacro with-indent ((&optional (spaces 4)) &body body)
  "Increase the indentation level in BODY by SPACES"
  `(let ((*indent* (+ *indent* ,spaces)))
     ,@body))

(defun xml-out (x &key (indent t))
  "Write X to XML output, escaped and optionally indented"
  (when indent (indent))
  (write-escaped (format nil "~A" x) *xml-output-stream*))

(defun xml-as-is (x &key (indent t))
  "Write X to XML output, unescaped and optionally indented"
  (when indent (indent))
  (format *xml-output-stream* "~A" x))

(defun start-tag (name &optional attrs namespace)
  "Write a start tag to XML output"
  (indent)
  (format *xml-output-stream* "<~A~@[ xmlns=\"~A\"~]"
	  name namespace)
  (dolist (attr attrs)
    (write-char #\Space *xml-output-stream*)
    (write-string (first attr) *xml-output-stream*)
    (write-string "=\"" *xml-output-stream*)
    (xml-out (second attr) :indent nil)
    (write-char #\" *xml-output-stream*))
  (write-char #\> *xml-output-stream*))

(defun end-tag (name)
  "Write en ending tag to XML output"
  (indent)
  (format *xml-output-stream* "</~A>" name))

;; High-level XML output
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-tag ((name &optional attrs namespace) &body body)
  "Wrap BODY in a tag. BODY is indented, and linebreaks are added."
  (once-only (name)
    `(progn
      (fresh-line *xml-output-stream*)
      (start-tag ,name ,attrs ,namespace)
      (terpri *xml-output-stream*)
      (with-indent ()
	,@body)
      (terpri *xml-output-stream*)
      (end-tag ,name))))

(defmacro with-simple-tag ((name &optional attrs namespace) &body body)
  "Like WITH-TAG, but without the linebreaks."
  (once-only (name)
    `(progn
      (fresh-line *xml-output-stream*)
      (start-tag ,name ,attrs ,namespace)
      (let ((*indent* 0))
	,@body
	(end-tag ,name)))))

(defmacro with-xml-output ((stream &key (encoding "ISO-8859-1")) &body body)
  "Wrap XML output on STREAM with the necessary XML heading information"
  `(let ((*xml-output-stream* ,stream))
     (format *xml-output-stream* "<?xml version=\"1.0\" encoding=~S?>~%"
 	     ,encoding)
     ,@body))

(defun simple-tag (name content &optional attrs namespace)
  "Emit a simple tag with given content"
  (with-simple-tag (name attrs namespace)
    (xml-out content)))

(defun emit-simple-tags (&rest tags-plist)
  "Given a plist mapping tag names to values (or nil), emit tags in
the order given, skipping ones with nil values. Tag names are
downcased unless they're passed as strings."
  (loop for (name tag) on tags-plist by #'cddr
	do (when tag
	     (simple-tag (format nil (if (symbolp name)
					 "~(~A~)"
					 "~A")
				 name) tag))))

;; Here is some example code. It writes a simple person description to
;; standard output, using most of the ways of doing output.

#+nil
(with-xml-output (*standard-output*)
  (with-tag ("person" '(("age" "19")))
    (with-simple-tag ("firstName")
      (xml-out "Peter"))
    (simple-tag "lastName" "Scott")
    (emit-simple-tags :age 17
		      :school "Iowa State Univeristy"
		      "mixedCaseTag" "Check out the mixed case!"
		      "notShown" nil)))