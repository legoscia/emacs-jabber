;;; Test the client by capturing its input and output into a virtual
;;; jabber server.  This is not a test in itself, but a framework for
;;; actual tests.

(require 'jabber)
(require 'cl)

(defvar jabberd-stanza-handlers '(jabberd-sasl jabberd-iq)
  "List of stanza handler hooks.
These functions are called in order with two arguments, the
client FSM and the stanza, until one function returns non-nil,
indicating that it has handled the stanza.")

(defvar jabberd-iq-get-handlers
  '(("jabber:iq:roster" . jabberd-iq-empty-success)
    ("jabber:iq:auth" . jabberd-iq-auth-get))
  "Alist of handlers for IQ get stanzas.
The key is the namespace of the request (a string), and the value
is a function to handle the request.  The function takes two
arguments, the client FSM and the stanza.")

(defvar jabberd-iq-set-handlers
  '(("urn:ietf:params:xml:ns:xmpp-bind" . jabberd-iq-bind)
    ("urn:ietf:params:xml:ns:xmpp-session" . jabberd-iq-empty-success)
    ("jabber:iq:auth" . jabberd-iq-empty-success))
  "Alist of handlers for IQ set stanzas.
The key is the namespace of the request (a string), and the value
is a function to handle the request.  The function takes two
arguments, the client FSM and the stanza.")

(defun jabberd-connect ()
  (setq *jabber-virtual-server-function* #'jabberd-handle)
  (jabber-connect "romeo" "montague.net" nil nil "foo" nil nil 'virtual))

(defun jabberd-handle (fsm text)
  ;; First, parse stanzas from text into sexps.
  (let (stanzas)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      ;; Skip processing directive
      (when (looking-at "<\\?xml[^?]*\\?>")
	(delete-region (match-beginning 0) (match-end 0)))
      (catch 'unfinished
	(while t
	 (push
	  (if (prog1
		  (looking-at "<stream:stream")
		(jabber-xml-skip-tag-forward t))
	      ;; Stream start - just leave as a string
	      (delete-and-extract-region (point-min) (point))
	    ;; Normal stanza
	    (prog1
		(car (xml-parse-region (point-min) (point)))
	      (delete-region (point-min) (point))))
	  stanzas)))
      ;; Delete whitespace - it has already been skipped over by
      ;; jabber-xml-skip-tag-forward
      (let ((whitespace-starts
	     (save-excursion (skip-chars-backward " \t\r\n") (point))))
	(delete-region whitespace-starts (point)))
      (unless (= (buffer-size) 0)
	(error "Couldn't parse outgoing XML: %S; %S remaining" text (buffer-string))))
    (setq stanzas (nreverse stanzas))

    ;; Now, let's handle the stanza(s).
    (dolist (stanza stanzas)
      (cond
       ((stringp stanza)
	;; "Send" a stream start in return.
	(fsm-send fsm (list :stream-start "42" "1.0"))
	;; If we have a stream start, see whether it wants XMPP 1.0.
	;; If so, send <stream:features>.
	(when (string-match "version=[\"']" stanza)
	  (jabberd-send fsm
			'(features
			  ((xmlns . "http://etherx.jabber.org/streams"))
			  ;; Interesting implementation details
			  ;; of jabber.el permit us to send all
			  ;; features at once, without caring about
			  ;; which step we are at.
			  (mechanisms 
			   ((xmlns . "urn:ietf:params:xml:ns:xmpp-sasl"))
			   (mechanism () "DIGEST-MD5"))
			  (bind ((xmlns . "urn:ietf:params:xml:ns:xmpp-bind")))
			  (session ((xmlns . "urn:ietf:params:xml:ns:xmpp-session")))))))
       (t
	(run-hook-with-args-until-success 'jabberd-stanza-handlers fsm stanza))))))

(defun jabberd-send (fsm stanza)
  (jabber-log-xml fsm "receive" stanza)
  (fsm-send fsm (list :stanza stanza)))

(defun jabberd-sasl (fsm stanza)
  "Pretend to authenticate the client by SASL."
  (when (eq (jabber-xml-node-name stanza) 'auth)
    (jabberd-send fsm '(success ((xmlns . "urn:ietf:params:xml:ns:xmpp-sasl"))))
    t))

(defun jabberd-iq (fsm stanza)
  "Handle IQs from the client."
  (when (eq (jabber-xml-node-name stanza) 'iq)
    (jabber-xml-let-attributes (type id) stanza
      (cond
       ((member type '("get" "set"))
	(let* ((table (if (string= type "get")
			  jabberd-iq-get-handlers
			jabberd-iq-set-handlers))
	       (ns (jabber-iq-xmlns stanza))
	       (function (cdr (assoc ns table))))
	  (when function
	    (funcall function fsm stanza)))))
      t)))

(defun jabberd-iq-empty-success (fsm stanza)
  "Send an empty IQ result to STANZA."
  (jabber-xml-let-attributes (id) stanza
    (jabberd-send
     fsm
     `(iq ((type . "result") (id . ,id))))))

(defun jabberd-iq-bind (fsm stanza)
  "Do resource binding for the virtual server."
  (let ((id (jabber-xml-get-attribute stanza 'id)))
    (jabberd-send
     fsm
     `(iq ((type . "result") (id . ,id))
	  (bind ((xmlns . "urn:ietf:params:xml:ns:xmpp-bind"))
		(jid () "romeo@montague.net/Orchard"))))))

(defun jabberd-iq-auth-get (fsm stanza)
  (jabber-xml-let-attributes (id) stanza
    (jabberd-send
     fsm
     `(iq ((type . "result") (id . ,id))
	  (query ((xmlns . "jabber:iq:auth"))
		 (username) (password) (digest) (resource))))))

(provide 'jabberd)
