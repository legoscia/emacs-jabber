(require 'jabberd)
(require 'cl)

(setq jabber-roster-show-bindings nil)

;; jabber-post-connect-hooks is run after the roster has been drawn
;; for the first time - but jabber-send-presence will redraw the
;; roster buffer after sending initial presence!  Make sure we check
;; the roster buffer after that has happened, so that the roster
;; buffer displays "Online" for ourselves already.
(add-hook 'jabber-post-connect-hooks 'rd-check-roster-buffer :append)

(defvar rd-roster-string nil)

(defun rd-check-roster-buffer (&optional _jc)
  (with-current-buffer jabber-roster-buffer
    (let ((contents (buffer-string)))
      (set-text-properties 0 (length contents) () contents)
      (setq rd-roster-string contents))))

(defun rd-compare (title expected)
  (princ title)
  (princ "...")
  (let ((result (mismatch rd-roster-string expected)))
    (if (null result)
	(princ "match\n")
      (princ "mismatch!  Expected:\n")
      (prin1 expected)
      (princ "\nBut got:\n")
      (prin1 (substring rd-roster-string 0 result))
      (princ " ***mismatch here*** ")
      (prin1 (substring rd-roster-string result))
      (error "Mismatch"))))

(jabberd-connect)

(with-timeout (5 (progn
		   (princ (with-current-buffer "*fsm-debug*" (buffer-string)))
		   (error "Timeout")))
  (while (not (and rd-roster-string (equal "" *jabber-current-show*)))
    (sit-for 0.1)))

(rd-compare
 "Empty roster"
 (concat
  "Jabber roster\n"
  "__________________________________\n"
  "\n"
  " - Online -\n"
  "romeo@montague.net\n"
  "__________________________________\n"
  "\n"
  "__________________________________\n"
  "\n"))

(jabber-process-input
 (car jabber-connections)
 '(iq ((type . "set"))
      (query ((xmlns . "jabber:iq:roster"))
	     (item ((jid . "juliet@capulet.com"))))))

(rd-check-roster-buffer)

(rd-compare
 "One contact"
 (concat
  "Jabber roster\n"
  "__________________________________\n"
  "\n"
  " - Online -\n"
  "romeo@montague.net\n"
  "__________________________________\n"
  "\n"
  "other\n"
  "     juliet@capulet.com            Offline   \n"
  "__________________________________\n"
  "\n"))
