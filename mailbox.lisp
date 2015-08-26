;; This is an example of something you might use the RSS emitter for
;; if you were writing a web-based email program. Google's Gmail does
;; something like this, so I was inspired.

(in-package :xml-emitter)

;; The email structure is deliberately simplistic.
(defstruct email
  from subject body date id)

;; In a real email app, we would get this from an SMTP server or a
;; database or something.
(defparameter *inbox* (list (make-email
			     :from "Peter Scott <sketerpot@gmail.com>"
			     :subject "Queue"
			     :date "Sat, 07 Sep 2002 00:00:01 GMT"
			     :id 34834)
			    (make-email
			     :from "Dave Pearson <i54jsw2@hotmail.com>"
			     :subject "THIS REALLY WORKS SKETERPOT!!!"
			     :date "Tue, 25 Sep 1987 13:42:41 GMT"
			     :id 34833)
			    (make-email
			     :from "Bubs <bubs@homestarrunner.com>"
			     :subject "Bubs' Weekly Spamvertisement"
			     :date "Wed, 14 Aug 2003 09:23:07 GMT"
			     :id 34832)))

(defun emit-inbox-feed (username stream)
  "Emit an RSS 2.0 feed for *INBOX* to STREAM, marking it as belonging
to USERNAME."
  (with-rss2 (stream)
    ;; Emit the header, with information about the feed
    (rss-channel-header (format nil "Inbox for ~A" username)
			(format nil "http://rss.mywebmail.com/~A.rss"
				username)
			:description "Your email inbox")
    (dolist (email *inbox*)
      ;; Emit an entry for a single email
      (rss-item (email-subject email)
		:link (format nil "http://www.mywebmail.com/viewemail?id=~A"
			      (email-id email))
		:author (email-from email)
		:pubDate (email-date email)
		:description (email-body email)))))

;; Evaluate this, and you should get the output below.
(emit-inbox-feed "sketerpot" *standard-output*)

#|
<?xml version="1.0" encoding="ISO-8859-1"?>
<rss version="2.0">
    <channel>
        <title>Inbox for sketerpot</title>
        <link>http://rss.mywebmail.com/sketerpot.rss</link>
        <description>Your email inbox</description>
        <language>en-us</language>
        <item>
            <title>Queue</title>
            <link>http://www.mywebmail.com/viewemail?id=34834</link>
            <author>Peter Scott &lt;sketerpot@gmail.com&gt;</author>
            <pubDate>Sat, 07 Sep 2002 00:00:01 GMT</pubDate>
        </item>
        <item>
            <title>THIS REALLY WORKS SKETERPOT!!!</title>
            <link>http://www.mywebmail.com/viewemail?id=34833</link>
            <author>Dave Pearson &lt;i54jsw2@hotmail.com&gt;</author>
            <pubDate>Tue, 25 Sep 1987 13:42:41 GMT</pubDate>
        </item>
        <item>
            <title>Bubs&apos; Weekly Spamvertisement</title>
            <link>http://www.mywebmail.com/viewemail?id=34832</link>
            <author>Bubs &lt;bubs@homestarrunner.com&gt;</author>
            <pubDate>Wed, 14 Aug 2003 09:23:07 GMT</pubDate>
        </item>
    </channel>
</rss>
|#