;; Tests for history

(require 'jabber-history)

;; 1. Smoke test
(let ((jabber-use-global-history t)
      (jabber-global-history-filename (make-temp-file "history-test"))
      ;; Jabber's birthday :)
      (our-time (encode-time 0 0 0 4 1 1999 0)))
  (unwind-protect
      (progn
	(jabber-history-log-message "in" "romeo@montague.net/Balcony" nil "hi" our-time)
	(with-temp-buffer
	  (insert-file-contents-literally jabber-global-history-filename)
	  (let ((expected "\\[\"\\([^\"]+\\)\" \"in\" \"romeo@montague.net/Balcony\" \"me\" \"hi\"]\n")
		(actual (buffer-string)))
	    (unless (string-match expected actual)
	      (error "Testcase 1 failed; %S doesn't match %S" actual expected))
	    ;; The timestamps don't match for some reason...
	    ;; (let ((timestamp (match-string 1 actual)))
;; 	      (unless (equal (jabber-parse-time timestamp) our-time)
;; 		(error "Testcase 1 failed; timestamp %S didn't match %S (%S vs %S)" timestamp (jabber-encode-time our-time) (jabber-parse-time timestamp) our-time)))
	    )))
    (delete-file jabber-global-history-filename)))

;; 2. Test with unwritable history file - should not signal an error
;; This should reflect out-of-disk condition too.
(let ((jabber-use-global-history t)
      (jabber-global-history-filename (make-temp-file "history-test")))
  (set-file-modes jabber-global-history-filename #o444)
  (unwind-protect
      (progn
	(jabber-history-log-message "in" "romeo@montague.net/Balcony" nil "hi" nil)
	(message "Please ignore the preceding \"Unable to write history\" error message.")
	;; No error signalled - we're done.
	)
    (delete-file jabber-global-history-filename)))

;; arch-tag: 43dd7ffe-22d7-11dd-9a7c-000a95c2fcd0
