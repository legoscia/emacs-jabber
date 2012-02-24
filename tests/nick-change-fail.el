;;; When the user tries to change nickname in an MUC room, and the
;;; server denies this, we should detect this instead of believing
;;; that the user was thrown out of the room.

(require 'jabberd)

(defconst ncf-room-name "orchard@romeo-and-juliet.shakespeare.lit"
  "The MUC room used for this test.")

(defun ncf-presence (fsm stanza)
  "Stanza handler.
This function is a very simple MUC implementation.  It allows a user
to enter the room named by `ncf-room-name' with the nick \"Romeo\"."
  (jabber-xml-let-attributes (to) stanza
    (when (and (eq (jabber-xml-node-name stanza) 'presence)
	       (string= (jabber-jid-user to) ncf-room-name))
      (let ((nick (jabber-jid-resource to)))
	;; Allow only the nick Romeo
	(if (string= nick "Romeo")
	    (jabberd-send fsm
			  `(presence ((from . ,to))
				     (x ((xmlns . "http://jabber.org/protocol/muc#user"))
					(item ((affiliation . "none")
					       (role . "participant"))))))
	  (jabberd-send fsm
			`(presence ((from . ,to)
				    (type . "error"))
				   (x ((xmlns . "http://jabber.org/protocol/muc#user")))
				   (error ((code . "409") (type . "cancel"))
					  (conflict ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))))))))))

(add-hook 'jabberd-stanza-handlers 'ncf-presence)
(add-hook 'jabber-post-connect-hooks 'ncf-do)
(setq jabber-muc-disable-disco-check t)
(setq jabber-debug-log-xml t)

(defvar ncf-done nil)
;; We need an extra variable for the error, as errors from timers are
;; ignored.
(defvar ncf-error nil)

(defun ncf-assert (assert-this format &rest args)
  (unless assert-this
    (let ((msg (apply #'format format args)))
      (setq ncf-error msg)
      (error "%s" msg))))

(defun ncf-do (jc)
  (setq ncf-done t)

  (jabber-muc-join jc ncf-room-name "Romeo")
  ;; We need a delay here, so that the client can process the response
  ;; stanza.
  (sit-for 0.01)
  (let ((buffer (jabber-muc-get-buffer ncf-room-name)))
    (ncf-assert (get-buffer buffer) "Couldn't enter MUC room")
    (ncf-assert *jabber-active-groupchats* "Entering room not recorded")

    ;; Now, do an unallowed nickname change.
    (jabber-muc-join jc ncf-room-name "Mercutio")
    (sit-for 0.01)

    ;; We should still consider ourselves to be in the room as Romeo
    (ncf-assert (assoc ncf-room-name *jabber-active-groupchats*)
		"We thought we left the room, but we didn't")
    (ncf-assert (string= (cdr (assoc ncf-room-name *jabber-active-groupchats*)) "Romeo")
       "We thought we changed nickname, but we didn't")))

(jabberd-connect)

(with-timeout (5 (error "Timeout"))
  (while (not ncf-done)
    (sit-for 0.1)))
(when ncf-error
  (princ 
   (format
    "nick-change-fail test FAILED: %s

" ncf-error))
  (princ "Conversation was:\n")
  (with-current-buffer "*-jabber-xml-log-romeo@montague.net-*"
    (princ (buffer-string)))
  (let ((muc-buffer (get-buffer (jabber-muc-get-buffer ncf-room-name))))
    (if muc-buffer
	(with-current-buffer muc-buffer
	  (princ "Contents of groupchat buffer:\n")
	  (princ (buffer-string)))
      (princ "Groupchat buffer not created.\n")))
  (kill-emacs 1))
