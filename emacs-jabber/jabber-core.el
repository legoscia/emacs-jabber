;; jabber-core.el - core functions

;; Copyright (C) 2003, 2004, 2007 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net

;; SSL-Connection Parts:
;; Copyright (C) 2005 - Georg Lehner - jorge@magma.com.ni

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(eval-when-compile (require 'cl))

(require 'jabber-util)
(require 'jabber-logon)
(require 'jabber-conn)
(require 'fsm)

(require 'jabber-sasl)

(defvar jabber-connections nil
  "List of jabber-connection FSMs.")

(defvar *jabber-roster* nil
  "the roster list")

(defvar jabber-jid-obarray (make-vector 127 0)
  "obarray for keeping JIDs")

(defvar *jabber-connected* nil
  "boolean - are we connected")

(defvar *jabber-authenticated* nil
  "boolean - are we authenticated")

(defvar *jabber-encrypted* nil
  "boolean - is the connection encrypted")

(defvar *jabber-disconnecting* nil
  "boolean - are we in the process of disconnecting by free will")

(defvar jabber-message-chain nil
  "Incoming messages are sent to these functions, in order.")

(defvar jabber-iq-chain nil
  "Incoming infoqueries are sent to these functions, in order.")

(defvar jabber-presence-chain nil
  "Incoming presence notifications are sent to these functions, in order.")

(defvar jabber-stream-error-chain '(jabber-process-stream-error)
  "Stream errors are sent to these functions, in order")

(defvar jabber-choked-count 0
  "Number of successive times that the process buffer has been nonempty.")

(defvar jabber-choked-timer nil)

(defgroup jabber-core nil "customize core functionality"
  :group 'jabber)

(defcustom jabber-post-connect-hooks '(jabber-send-default-presence
				       jabber-muc-autojoin)
  "*Hooks run after successful connection and authentication.
The functions should accept one argument, the connection object."
  :type 'hook
  :group 'jabber-core)

(defcustom jabber-pre-disconnect-hook nil
  "*Hooks run just before voluntary disconnection
This might be due to failed authentication.  Check `*jabber-authenticated*'."
  :type 'hook
  :group 'jabber-core)

(defcustom jabber-lost-connection-hook nil
  "*Hooks run after involuntary disconnection"
  :type 'hook
  :group 'jabber-core)

(defcustom jabber-post-disconnect-hook nil
  "*Hooks run after disconnection"
  :type 'hook
  :group 'jabber-core)

(defcustom jabber-roster-buffer "*-jabber-*"
  "The name of the roster buffer"
  :type 'string
  :group 'jabber-core)

(defvar jabber-process-buffer " *-jabber-process-*"
  "The name of the process buffer")

(defcustom jabber-use-sasl t
  "If non-nil, use SASL if possible.
SASL will still not be used if the library for it is missing or
if the server doesn't support it.

Disabling this shouldn't be necessary, but it may solve certain
problems."
  :type 'boolean
  :group 'jabber-core)

(defsubst jabber-have-sasl-p ()
  "Return non-nil if SASL functions are available."
  (featurep 'sasl))

(defun jabber-connect (username server resource &optional registerp)
  "connect to the jabber server and start a jabber xml stream
With prefix argument, register a new account."
  (interactive
   (let* ((default (when (and jabber-username jabber-server)
			  (if jabber-resource
			      (format "%s@%s/%s"
				      jabber-username 
				      jabber-server
				      jabber-resource)
			    (format "%s@%s"
				    jabber-username
				    jabber-server))))
	  (jid (read-string 
		(if default
		    (format "Enter your JID: (default %s) " default)
		  "Enter your JID: ")
		nil nil default)))
     (list (jabber-jid-username jid)
	   (jabber-jid-server jid)
	   (or (jabber-jid-resource jid) jabber-resource)
	   current-prefix-arg)))
  ;; XXX: better way of specifying which account(s) to connect to.
  (if (member (list username
		    server)
	      (mapcar
	       (lambda (c)
		 (let ((data (fsm-get-state-data c)))
		   (list (plist-get data :username)
			 (plist-get data :server))))
	       jabber-connections))
      (message "Already connected to %s@%s"
	       username server)
    (setq *jabber-authenticated* nil)
    ;;(jabber-clear-roster)
    (jabber-reset-choked)

    (push (start-jabber-connection username 
				   server
				   resource
				   registerp)
	  jabber-connections)))

(define-state-machine jabber-connection
  :start ((username server resource &optional registerp)
	  "Start a Jabber connection."
	  (let ((connect-function
		 (jabber-get-connect-function jabber-connection-type))
		(send-function
		 (jabber-get-send-function jabber-connection-type)))
	    (funcall connect-function fsm server)

	    (list :connecting 
		  (list :send-function send-function
			:username username
			:server server
			:resource resource
			:registerp registerp)))))

(define-enter-state jabber-connection nil
  (fsm state-data)
  ;; `nil' is the error state.  Remove the connection from the list.
  (setq jabber-connections
	(delq fsm jabber-connections))
  ;; Remove lost connections from the roster buffer.
  (jabber-display-roster)
  (list state-data nil))

;; There is no `define-state' for `nil', since any message received
;; there is an error.  They will be silently ignored, and only logged
;; in *fsm-debug*.

(define-state jabber-connection :connecting
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:connected
     (let ((connection (cadr event))
	   (registerp (plist-get state-data :registerp)))
     
       ;; TLS connections leave data in the process buffer, which
       ;; the XML parser will choke on.
       (with-current-buffer (process-buffer connection)
	 (erase-buffer))

       ;; state-data is a list here, so we can use nconc for appending
       ;; without losing the correct reference.
       (nconc state-data (list :connection connection))

       (set-process-filter connection (fsm-make-filter fsm))
       (set-process-sentinel connection (fsm-make-sentinel fsm))

       (list :connected state-data)))

    (:connection-failed
     (message "Jabber connection failed")
     (list nil state-data))))

(define-enter-state jabber-connection :connected
  (fsm state-data)

  (jabber-send-stream-header fsm)
  
  (setq jabber-choked-timer
	(run-with-timer 5 5 #'jabber-check-choked))

  ;;XXX: why is this here?  I'll try commenting it out...
  ;;(accept-process-output *jabber-connection*)

  ;; Next thing happening is the server sending its own <stream:stream> start tag.
  
  (setq *jabber-connected* t)
  (list state-data nil))

(define-state jabber-connection :connected
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :connected state-data)))

    (:sentinel
     (message "Jabber connection unexpectedly closed")
     (list nil state-data))

    (:stream-start
     (let ((session-id (cadr event))
	   (stream-version (car (cddr event))))
       ;; the stream feature is only sent if the initiating entity has
       ;; sent 1.0 in the stream header. if sasl is not supported then
       ;; we don't send 1.0 in the header and therefore we shouldn't wait
       ;; even if 1.0 is present in the receiving stream.
       (cond
	;; Wait for stream features?
	((and stream-version
	      (>= (string-to-number stream-version) 1.0)
	      jabber-use-sasl
	      (jabber-have-sasl-p))
	 ;; Stay in same state...
	 (list :connected state-data))
	;; Register account?
	((plist-get state-data :registerp)
	 ;; XXX: require encryption for registration?
	 (list :register-account state-data))
	;; Legacy authentication?
	(t
	 (list :legacy-auth (plist-put state-data :session-id session-id))))))

    (:stanza
     (let ((stanza (cadr event)))
       ;; At this stage, we only expect a stream:features stanza.
       (unless (eq (jabber-xml-node-name stanza) 'stream:features)
	 (error "Unexpected stanza %s" stanza))

       (cond
	((jabber-xml-get-children stanza 'starttls)
	 (list :starttls state-data))
	;; XXX: require encryption for registration?
	((plist-get state-data :registerp)
	 ;; We could check for the <register/> element in stream
	 ;; features, but as a client we would only lose by doing
	 ;; that.
	 (list :register-account state-data))
	(t
	 (list :sasl-auth (plist-put state-data :stream-features stanza))))))))

(define-enter-state jabber-connection :starttls
  (fsm state-data)
  (jabber-starttls-initiate fsm)
  (list state-data nil))

(define-state jabber-connection :starttls
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :starttls state-data)))

    (:sentinel
     (message "Jabber connection unexpectedly closed")
     (list nil state-data))

    (:stanza
     (if (jabber-starttls-process-input fsm (cadr event))
	 ;; Connection is encrypted.  Send a stream tag again.
	 ;; XXX: note encryptedness of connection.
	 (list :connected state-data)
       (message "STARTTLS negotiation failed")
       (list nil state-data)))))

(define-enter-state jabber-connection :register-account
  (fsm state-data)
  (jabber-get-register fsm nil)
  (list state-data nil))

(define-state jabber-connection :register-account
  (fsm state-data event callback)
  ;; The connection will be closed in jabber-register
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :register-account state-data)))

    (:sentinel
     (message "Jabber connection unexpectedly closed")
     (list nil state-data))

    (:stanza
     (jabber-process-input fsm (cadr event))
     (list :register-account state-data))))

(define-enter-state jabber-connection :legacy-auth
  (fsm state-data)
  (jabber-get-auth fsm (plist-get state-data :server)
		   (plist-get state-data :session-id))
  (list state-data nil))

(define-state jabber-connection :legacy-auth
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :legacy-auth state-data)))

    (:sentinel
     (message "Jabber connection unexpectedly closed")
     (list nil state-data))

    (:stanza
     (jabber-process-input fsm (cadr event))
     (list :legacy-auth state-data))

    (:authentication-success
     (list :session-established state-data))

    (:authentication-failure
     ;; jabber-logon has already displayed a message
     (list nil state-data))))

(define-enter-state jabber-connection :sasl-auth
  (fsm state-data)
  (let ((new-state-data
	 (append state-data
		 (list :sasl-data 
		       (jabber-sasl-start-auth 
			fsm
			(plist-get state-data
				   :stream-features))))))
    (list new-state-data nil)))

(define-state jabber-connection :sasl-auth
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :sasl-auth state-data)))

    (:sentinel
     (message "Jabber connection unexpectedly closed")
     (list nil state-data))

    (:stanza
     (let ((new-sasl-data
	    (jabber-sasl-process-input 
	     fsm (cadr event) 
	     (plist-get state-data :sasl-data))))
       (list :sasl-auth (plist-put state-data :sasl-data new-sasl-data))))

    (:use-legacy-auth-instead
     (list :legacy-auth (plist-put state-data :sasl-data nil)))

    (:authentication-success
     (list :bind (plist-put state-data :sasl-data nil)))

    (:authentication-failure
     ;; jabber-sasl has already displayed a message
     (list nil state-data))))

(define-enter-state jabber-connection :bind
  (fsm state-data)
  (jabber-send-stream-header fsm)
  (list state-data nil))

(define-state jabber-connection :bind
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :bind state-data)))

    (:sentinel
     (message "Jabber connection unexpectedly closed")
     (list nil state-data))

    (:stream-start
     ;; we wait for stream features...
     (list :bind state-data))

    (:stanza
     (let ((stanza (cadr event)))
     (cond
      ((eq (jabber-xml-node-name stanza) 'stream:features)
       (if (and (jabber-xml-get-children stanza 'bind)
		(jabber-xml-get-children stanza 'session))
	   (labels
	       ((handle-bind 
		 (jc xml-data success)
		 (fsm-send jc (list
			       (if success :bind-success :bind-failure)
			       xml-data))))
	     ;; So let's bind a resource.  We can either pick a resource ourselves,
	     ;; or have the server pick one for us.
	     (jabber-send-iq fsm nil "set"
			     `(bind ((xmlns . "urn:ietf:params:xml:ns:xmpp-bind"))
				    (resource () ,jabber-resource))
			     #'handle-bind t
			     #'handle-bind nil)
	     (list :bind state-data))
	 (message "Server doesn't permit resource binding and session establishing")
	 (list nil state-data)))
      (t
       (jabber-process-input fsm (cadr event))
       (list :bind state-data)))))

    (:bind-success
     (let ((jid (jabber-xml-path (cadr event) '(bind jid ""))))
       ;; Maybe this isn't the JID we asked for.
       (plist-put state-data :username (jabber-jid-username jid))
       (plist-put state-data :server (jabber-jid-server jid))
       (plist-put state-data :resource (jabber-jid-resource jid)))

     ;; Been there, done that.  Time to establish a session.
     (labels 
	 ((handle-session
	   (jc xml-data success)
	   (fsm-send jc (list
			 (if success :session-success :session-failure)
			 xml-data))))
       (jabber-send-iq fsm nil "set"
		       '(session ((xmlns . "urn:ietf:params:xml:ns:xmpp-session")))
		       #'handle-session t
		       #'handle-session nil)
       (list :bind state-data)))

    (:session-success
     ;; We have a session
     (list :session-established state-data))

    (:bind-failure
     (message "Resource binding failed: %s" 
	      (jabber-parse-error
	       (jabber-iq-error (cadr event))))
     (list nil state-data))

    (:session-failure
     (message "Session establishing failed: %s"
	      (jabber-parse-error
	       (jabber-iq-error (cadr event))))
     (list nil state-data))))

(define-enter-state jabber-connection :session-established
  (fsm state-data)
  (jabber-send-iq fsm nil
		  "get" 
		  '(query ((xmlns . "jabber:iq:roster")))
		  #'jabber-process-roster 'initial
		  #'jabber-report-success "Roster retrieval")
  (list state-data nil))

(define-state jabber-connection :session-established
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :session-established state-data)))

    (:sentinel
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (run-hooks 'jabber-lost-connection-hook)
       (message "%s@%s/%s: connection lost: `%s'"
		(plist-get state-data :username)
		(plist-get state-data :server)
		(plist-get state-data :resource)
		string)
       (list nil state-data)))

    (:stanza
     (jabber-process-input fsm (cadr event))
     (list :session-established state-data))))

(defun jabber-disconnect ()
  "Disconnect from all Jabber servers."
  (interactive)
  ;; XXX: this function is slightly out of sync with the rest of the
  ;; FSM remake.
  (unless *jabber-disconnecting*	; avoid reentry
    (let ((*jabber-disconnecting* t))
      (dolist (c jabber-connections)
	(jabber-disconnect-one c t))
      (setq jabber-connections nil)

      (jabber-disconnected)
      (when (interactive-p)
	(message "Disconnected from Jabber server")))))

(defun jabber-disconnect-one (jc &optional dont-redisplay)
  "Disconnect from one Jabber server.
If DONT-REDISPLAY is non-nil, don't update roster buffer."
  (interactive (list (jabber-read-account)))
  ;;(run-hooks 'jabber-pre-disconnect-hook)
  (let ((process (plist-get
		  (fsm-get-state-data jc)
		  :connection)))
    (when (and process
	       (memq (process-status process) '(open run)))
      (jabber-send-string jc "</stream:stream>")
      ;; let the server close the stream
      (accept-process-output process 3)
      ;; and do it ourselves as well, just to be sure
      (delete-process process)))
  (setq jabber-connections (remq jc jabber-connections))
  (unless dont-redisplay
    (jabber-display-roster)))

(defun jabber-disconnected ()
  "Re-initialise jabber package variables.
Call this function after disconnection."
  (when jabber-choked-timer
    (jabber-cancel-timer jabber-choked-timer)
    (setq jabber-choked-timer nil))

  (when (get-buffer jabber-roster-buffer)
    (with-current-buffer (get-buffer jabber-roster-buffer)
      (let ((inhibit-read-only t))
	(erase-buffer))))

  (setq *jabber-connection* nil)
  (jabber-clear-roster)
  (setq *jabber-authenticated* nil)
  (setq *jabber-encrypted* nil)
  (setq *jabber-connected* nil)
  (setq *jabber-active-groupchats* nil)
  (run-hooks 'jabber-post-disconnect-hook))

(defun jabber-pre-filter (process string fsm)
  (with-current-buffer (process-buffer process)
    ;; Append new data
    (goto-char (point-max))
    (insert string)

    (unless (boundp 'jabber-filtering)
      (let (jabber-filtering)
	(jabber-filter process fsm)))))

(defun jabber-filter (process fsm)
  "the filter function for the jabber process"
  (with-current-buffer (process-buffer process)
    ;; Start from the beginning
    (goto-char (point-min))
    (let (xml-data)
      (loop 
       do
       ;; Skip whitespace
       (unless (zerop (skip-chars-forward " \t\r\n"))
	 (delete-region (point-min) (point)))
       ;; Skip processing directive
       (when (looking-at "<\\?xml[^?]*\\?>")
	 (delete-region (match-beginning 0) (match-end 0)))

       ;; Stream end?
       (when (looking-at "</stream:stream>")
	 (return (fsm-send fsm :stream-end)))

       ;; Stream header?
       (when (looking-at "<stream:stream[^>]*>")
	 (let ((stream-header (match-string 0))
	       (ending-at (match-end 0))
	       session-id stream-version)
	   ;; These regexps extract attribute values from the stream
	   ;; header, taking into account that the quotes may be either
	   ;; single or double quotes.
	   (setq session-id
		 (and (or (string-match "id='\\([^']+\\)'" stream-header)
			  (string-match "id=\"\\([^\"]+\\)\"" stream-header))
		      (jabber-unescape-xml (match-string 1 stream-header))))
	   (setq stream-version
		 (and (or
		       (string-match "version='\\([0-9.]+\\)'" stream-header)
		       (string-match "version=\"\\([0-9.]+\\)\"" stream-header))
		      (match-string 1 stream-header)))
	   (if jabber-debug-log-xml
	       (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
		 (save-excursion
		   (goto-char (point-max))
		   (insert (format "receive %S\n\n" stream-header)))))

	   ;; If the server is XMPP compliant, i.e. there is a version attribute
	   ;; and it's >= 1.0, there will be a stream:features tag shortly,
	   ;; so just wait for that.

	   (fsm-send fsm (list :stream-start session-id stream-version))
	 
	   (delete-region (point-min) ending-at)))
       
       ;; Normal tag

       ;; XXX: do these checks make sense?  If so, reinstate them.
       ;;(if (active-minibuffer-window)
       ;;    (run-with-idle-timer 0.01 nil #'jabber-filter process string)

       ;; This check is needed for xml.el of Emacs 21, as it chokes on
       ;; empty attribute values.
       (save-excursion
	 (while (search-forward-regexp " \\w+=''" nil t)
           (replace-match "")))
       
       (setq xml-data (and (catch 'unfinished
			     (jabber-xml-skip-tag-forward)
			     (> (point) (point-min)))
			   (xml-parse-region (point-min) (point))))
       (if xml-data
	   (jabber-reset-choked))

       while xml-data
       do
       ;; If there's a problem with writing the XML log,
       ;; make sure the stanza is delivered, at least.
       (condition-case e
	   (if jabber-debug-log-xml
	       (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
		 (save-excursion
		   (goto-char (point-max))
		   (insert (format "receive %S\n\n" (car xml-data))))))
	 (error
	  (ding)
	  (message "Couldn't write XML log: %s" (error-message-string e))
	  (sit-for 2)))
       (delete-region (point-min) (point))

       (fsm-send fsm (list :stanza (car xml-data)))
       ;; XXX: move this logic elsewhere
       ;; We explicitly don't catch errors in jabber-process-input,
       ;; to facilitate debugging.
       ;; (jabber-process-input (car xml-data))
       ))))

(defun jabber-reset-choked ()
  (setq jabber-choked-count 0))

(defun jabber-check-choked ()
  ;; "Choked" means that data is sitting in the process buffer
  ;; without being parsed, despite several attempts.
  (if (zerop (buffer-size (process-buffer *jabber-connection*)))
      (jabber-reset-choked)
    (incf jabber-choked-count)
    (if (and (> jabber-choked-count 3)
	     ;; Now we're definitely choked.  Take action.
	     ;; But ask user first.
	     (yes-or-no-p "jabber.el is severely confused.  Bail out? "))
	(run-with-idle-timer 0.1 nil 'jabber-choked-bail-out)
      (jabber-reset-choked))))

(defun jabber-choked-bail-out ()
  ;; So here we are.  Something in the process buffer prevents us
  ;; from continuing normally.  Let's die honorably by providing
  ;; bug report material.
  (with-current-buffer (generate-new-buffer "*jabber-bug*")
    (insert "jabber.el couldn't cope with the data received from the server.
This should never happen, but apparently it did.

The information below will be helpful in tracking down and fixing
the bug.  You may want to edit out any sensitive information.

Please go to
http://sourceforge.net/tracker/?group_id=88346&atid=586350 and
submit a bug report, including the information below.

")
    (goto-address)
    (emacs-version t)
    (insert "\n\nThe following couldn't be parsed:\n")
    (insert-buffer-substring (process-buffer *jabber-connection*))
    (switch-to-buffer (current-buffer)))
  (jabber-disconnect))

(defun jabber-process-input (jc xml-data)
  "process an incoming parsed tag"
  (let* ((tag (jabber-xml-node-name xml-data))
	 (functions (eval (cdr (assq tag '((iq . jabber-iq-chain)
					   (presence . jabber-presence-chain)
					   (message . jabber-message-chain)
					   (stream:error . jabber-stream-error-chain)))))))

    (dolist (f functions)
      (funcall f jc xml-data))))

(defun jabber-process-stream-error (jc xml-data)
  "Process an incoming stream error."
  (beep)
  (run-hooks 'jabber-lost-connection-hook)
  (message "Stream error, connection lost: %s" (jabber-parse-stream-error xml-data))
  (jabber-disconnect-one jc))

;; XXX: This function should probably die.  The roster is stored
;; inside the connection plists, and the obarray shouldn't be so big
;; that we need to clean it.
(defun jabber-clear-roster ()
  "Clean up the roster."
  ;; This is made complicated by the fact that the JIDs are symbols with properties.
  (mapatoms #'(lambda (x)
		(unintern x jabber-jid-obarray))
	    jabber-jid-obarray)
  (setq *jabber-roster* nil))

(defun jabber-send-sexp (jc sexp)
  "Send the xml corresponding to SEXP to connection JC."
  (condition-case e
      (if jabber-debug-log-xml
	  (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
	    (save-excursion
	      (goto-char (point-max))
	      (insert (format "sending %S\n\n" sexp)))))
    (error
     (ding)
     (message "Couldn't write XML log: %s" (error-message-string e))
     (sit-for 2)))
  (jabber-send-string jc (jabber-sexp2xml sexp)))

(defun jabber-send-stream-header (jc)
  "Send stream header to connection JC."
  (let ((stream-header
	 (concat "<?xml version='1.0'?><stream:stream to='" 
		 (plist-get (fsm-get-state-data jc) :server)
		 "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'"
		 ;; Not supporting SASL is not XMPP compliant,
		 ;; so don't pretend we are.
		 (if (and (jabber-have-sasl-p) jabber-use-sasl)
		     " version='1.0'"
		   "")
		 ">
")))
    (jabber-send-string jc stream-header)
    (when jabber-debug-log-xml
      (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
	(save-excursion
	  (goto-char (point-max))
	  (insert (format "sending %S\n\n" stream-header)))))))

(defun jabber-send-string (jc string)
  "Send STRING to the connection JC."
  (let* ((state-data (fsm-get-state-data jc))
	 (connection (plist-get state-data :connection))
	 (send-function (plist-get state-data :send-function)))
    (funcall send-function connection string)))

(provide 'jabber-core)

;;; arch-tag: 9d273ce6-c45a-447b-abf3-21d3ce73a51a
