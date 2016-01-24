;; Tests for jabber-xml-parse-next-stanza

(require 'jabber-xml)

(defun parse-it (text)
  (with-temp-buffer
    (insert text)
    (jabber-xml-parse-next-stanza)))

(unless (equal
	 (parse-it "<presence from='foo@example.com/resource' type='unavailable' to='bar@example.com'/>")
	 '((presence ((from . "foo@example.com/resource") (type . "unavailable") (to . "bar@example.com")))))
  (error "Testcase 1 failed"))

(unless (equal
	 (parse-it "<presence from='foo@example.com/resource' ")
	 nil)
  (error "Testcase 2 failed"))
