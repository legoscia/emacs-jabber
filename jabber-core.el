;; jabber-core.el - core functions

;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
;; Copyright (C) 2003, 2004 - Magnus Henoch - mange@freemail.hu

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

(require 'jabber-util)
(require 'jabber-logon)

;; SASL depends on FLIM.
(eval-and-compile
  (condition-case nil
      (require 'jabber-sasl)
    (error nil)))

(defvar *jabber-connection* nil
  "the process that does the actual connection")

(defvar *jabber-roster* nil
  "the roster list")

(defvar jabber-jid-obarray (make-vector 127 0)
  "obarray for keeping JIDs")

(defvar *jabber-connected* nil
  "boolean - are we connected")

(defvar *jabber-authenticated* nil
  "boolean - are we authenticated")

(defvar *jabber-disconnecting* nil
  "boolean - are we in the process of disconnecting by free will")

(defvar *xmlq* ""
  "a string with all the incoming xml that is waiting to be parsed")

(defvar jabber-register-p nil
  "Register a new account in this session?")

(defvar jabber-session-id nil
  "id of the current session")

(defvar jabber-stream-version nil
  "Stream version indicated by server")

(defvar jabber-register-p nil
  "Is account registration occurring in this session?")

(defvar jabber-call-on-connection nil
  "Function to be called on connection.
This is set by `jabber-connect' on each call, and later picked up in
`jabber-filter'.")

(defvar jabber-short-circuit-input nil
  "Function that receives all stanzas, instead of the usual ones.
Used for SASL authentication.")

(defvar jabber-message-chain nil
  "Incoming messages are sent to these functions, in order.")

(defvar jabber-iq-chain nil
  "Incoming infoqueries are sent to these functions, in order.")

(defvar jabber-presence-chain nil
  "Incoming presence notifications are sent to these functions, in order.")

(defvar jabber-stream-error-chain '(jabber-process-stream-error)
  "Stream errors are sent to these functions, in order")

(defgroup jabber-core nil "customize core functionality"
  :group 'jabber)

(defcustom jabber-post-connect-hook '(jabber-send-default-presence)
  "*Hooks run after successful connection and authentication."
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

(defsubst jabber-have-sasl-p ()
  "Return non-nil if SASL functions are available."
  (fboundp 'jabber-sasl-start-auth))

(defun jabber-connect (&optional registerp)
  "connect to the jabber server and start a jabber xml stream
With prefix argument, register a new account."
  (interactive "P")
  (if *jabber-connected*
      (message "Already connected")
    (setq *xmlq* "")
    (setq *jabber-authenticated* nil)
    (jabber-clear-roster)
    (let ((coding-system-for-read 'utf-8)
	  (coding-system-for-write 'utf-8))
      (setq *jabber-connection* (open-network-stream "jabber"
						     jabber-roster-buffer
						     (or jabber-network-server jabber-server)
						     jabber-port)))
    (set-process-filter *jabber-connection* #'jabber-filter)
    (set-process-sentinel *jabber-connection* #'jabber-sentinel)

    (setq jabber-short-circuit-input nil)
    (setq jabber-register-p registerp)

    (setq jabber-call-on-connection (if registerp
					#'(lambda (stream-features) (jabber-get-register jabber-server))
				      #'jabber-auth-somehow))
    (let ((stream-header (concat "<?xml version='1.0'?><stream:stream to='" 
				 jabber-server 
				 "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'"
				 ;; Not supporting SASL is not XMPP compliant,
				 ;; so don't pretend we are.
				 (if (jabber-have-sasl-p)
				     " version='1.0'"
				   "")
				 ">")))
      (process-send-string *jabber-connection*
			   stream-header)
      (if jabber-debug-log-xml
	  (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
	    (save-excursion
	      (goto-char (point-max))
	      (insert (format "sending %S\n\n" stream-header))))))
    ;; Next thing happening is the server sending its own <stream:stream> start tag.
    ;; That is handled in jabber-filter.

    (setq *jabber-connected* t)))

(defun jabber-auth-somehow (stream-features)
  "Start authentication with SASL if the server supports it,
otherwise JEP-0077.  The STREAM-FEATURES argument is the stream features
tag, or nil if we're connecting to a pre-XMPP server."
  (if (and stream-features
	   (jabber-have-sasl-p)
	   jabber-stream-version
	   (>= (string-to-number jabber-stream-version) 1.0))
      (jabber-sasl-start-auth stream-features)
    (jabber-get-auth jabber-server)))

(defun jabber-disconnect ()
  "disconnect from the jabber server and re-initialise the jabber package variables"
  (interactive)
  (unless *jabber-disconnecting*	; avoid reentry
    (let ((*jabber-disconnecting* t))
      (when (eq (process-status *jabber-connection*) 'open)
	(run-hooks 'jabber-pre-disconnect-hook)
	(process-send-string *jabber-connection* "</stream:stream>")
	;; let the server close the stream
	(accept-process-output *jabber-connection* 3)
	;; and do it ourselves as well, just to be sure
	(delete-process *jabber-connection*))
      (jabber-disconnected)
      (if (interactive-p)
	  (message "Disconnected from Jabber server")))))

(defun jabber-disconnected ()
  "Re-initialise jabber package variables.
Call this function after disconnection."
  (when (and (processp *jabber-connection*)
	     (process-buffer *jabber-connection*))
    (kill-buffer (process-buffer *jabber-connection*)))
  (setq *jabber-connection* nil)
  (jabber-clear-roster)
  (setq *xmlq* "")
  (setq *jabber-authenticated* nil)
  (setq *jabber-connected* nil)
  (setq *jabber-active-groupchats* nil)
  (setq jabber-session-id nil)
  (run-hooks 'jabber-post-disconnect-hook))

(defun jabber-sentinel (process event)
  "alert user about lost connection"
  (unless *jabber-disconnecting*
    (beep)
    (run-hooks 'jabber-lost-connection-hook)
    (message "Jabber connection lost: `%s'" event)
    (jabber-disconnected)))

(defun jabber-filter (process string)
  "the filter function for the jabber process"
  (cond
   ((string-match "^</stream:stream>" string)
    (jabber-disconnect))
   ((string-match "\\(<stream:stream[^>]*>\\)\\(.*\\)" string)
    (let ((stream-header (match-string 1 string))
	  (rest (match-string 2 string)))
      (setq jabber-session-id
	    (progn (string-match "id='\\([A-Za-z0-9]+\\)'" stream-header)
		   (match-string 1 stream-header)))
      (setq jabber-stream-version
	    (and (string-match "version='\\([0-9.]+\\)'" stream-header)
		 (match-string 1 stream-header)))
      (if jabber-debug-log-xml
	  (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
	    (save-excursion
	      (goto-char (point-max))
	      (insert (format "receive %S\n\n" string)))))

      ;; If the server is XMPP compliant, i.e. there is a version attribute
      ;; and it's >= 1.0, there will be a stream:features tag shortly,
      ;; so just wait for that.
      (unless (and jabber-stream-version
		   (>= (string-to-number jabber-stream-version) 1.0))
	;; Logon or register
	(funcall jabber-call-on-connection nil))

      ;; If we got more than the stream header, pass it on.
      (if (not (zerop (length rest)))
	  (jabber-filter process rest))))
   (t
    (if (active-minibuffer-window)
        (run-with-idle-timer 0.01 nil #'jabber-filter process string)
      (with-temp-buffer
        (setq *xmlq* (concat *xmlq* string))
        (if (string-match " \\w+=''" *xmlq*)
            (setq *xmlq* (replace-match "" nil nil *xmlq*)))
	(unwind-protect
	    (catch 'jabber-no-tag
	      (while (string-match "<\\([a-zA-Z0-9\:]+\\)[> ]" *xmlq*)
		(if (or (string-match (concat "<" (match-string 1 *xmlq*) "[^<>]*?/>") *xmlq*)
			(string-match (concat "<" (match-string 1 *xmlq*) ".*?>[^\0]+?</" (match-string 1 *xmlq*) ">") *xmlq*))
		    (progn
		      (insert (match-string 0 *xmlq*))
		      (goto-char (point-min))
		      (setq *xmlq* (substring *xmlq* (match-end 0)))
		      (let ((xml-data (xml-parse-region (point-min)
							(point-max))))
			(when xml-data
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
			  (jabber-process-input (car xml-data))))
		      (erase-buffer))
		  (throw 'jabber-no-tag t))))
	  ;; unwindforms of unwind-protect
	  (when (not (zerop (length *xmlq*)))
	    ;; If there is XML data remaining to be parsed,
	    ;; and an error aborted the loop, continue processing
	    ;; after one second.  Otherwise the data would sit in *xmlq*
	    ;; until the next time jabber-filter is called.
	    ;;
	    ;; We could just catch the error, but that would make noticing and
	    ;; debugging errors harder; "Error in process filter" is a good
	    ;; reminder that something's not right.  Concerning data loss,
	    ;; there's not much we can do here, as the error-causing stanza
	    ;; could be any kind of information.  The jabber-process-*
	    ;; functions should catch their own errors if they need to.
	    (run-with-idle-timer 1 nil #'jabber-filter process ""))))))))

(defun jabber-process-input (xml-data)
  "process an incoming parsed tag"
  (let* ((tag (jabber-xml-node-name xml-data))
	 (functions (eval (cdr (assq tag '((iq . jabber-iq-chain)
					   (presence . jabber-presence-chain)
					   (message . jabber-message-chain)
					   (stream:error . jabber-stream-error-chain)))))))
    ;; Special treatment of the stream:features tag.  The first time we get it,
    ;; it means that we should authenticate.  The second time, we should
    ;; establish a session.  (The zeroth time it's STARTTLS, but that's not
    ;; implemented yet.)
    (if (eq tag 'stream:features)
	(if *jabber-authenticated*
	    (jabber-bind-and-establish-session xml-data)
	  (funcall jabber-call-on-connection xml-data))
      (if jabber-short-circuit-input
	  (funcall jabber-short-circuit-input xml-data)
	(dolist (f functions)
	  (funcall f xml-data))))))

(defun jabber-process-stream-error (xml-data)
  "Process an incoming stream error."
  (let ((*jabber-disconnecting* t))
    (beep)
    (run-hooks 'jabber-lost-connection-hook)
    (message "Stream error, connection lost: %s" (jabber-parse-stream-error xml-data))
    (jabber-disconnected)))

(defun jabber-bind-and-establish-session (xml-data)
  ;; Now we have a stream:features tag.  We expect it to contain bind and
  ;; session tags.  If it doesn't, the server we are connecting to is no
  ;; IM server.
  (unless (and (jabber-xml-get-children xml-data 'bind)
	       (jabber-xml-get-children xml-data 'session))
    (jabber-disconnect)
    (error "Server doesn't permit resource binding and session establishing"))

  ;; So let's bind a resource.  We can either pick a resource ourselves,
  ;; or have the server pick one for us.
  (jabber-send-iq nil "set"
		  `(bind ((xmlns . "urn:ietf:params:xml:ns:xmpp-bind"))
			 (resource () ,jabber-resource))
		  #'jabber-process-bind t
		  #'jabber-process-bind nil))

(defun jabber-process-bind (xml-data successp)
  (unless successp
    (jabber-disconnect)
    (error "Resource binding failed: %s" 
	   (jabber-parse-error (car (jabber-xml-get-children xml-data 'error)))))

  (let ((jid (car
	      (jabber-xml-node-children
	       (car
		(jabber-xml-get-children
		 (jabber-iq-query xml-data) 'jid))))))
    ;; Maybe this isn't the resource we asked for.
    (setq jabber-resource (jabber-jid-resource jid)))

  ;; Been there, done that.  Time to establish a session.
  (jabber-send-iq nil "set"
		  '(session ((xmlns . "urn:ietf:params:xml:ns:xmpp-session")))
		  #'jabber-process-session t
		  #'jabber-process-session nil))

(defun jabber-process-session (xml-data successp)
  (unless successp
    (jabber-disconnect)
    (error "Session establishing failed: %s" 
	   (jabber-parse-error (car (jabber-xml-get-children xml-data 'error)))))

  ;; Now, request roster.
  (jabber-send-iq nil
		  "get" 
		  '(query ((xmlns . "jabber:iq:roster")))
		  #'jabber-process-roster 'initial
		  #'jabber-report-success "Roster retrieval")

  (run-hooks 'jabber-post-connect-hook))

(defun jabber-clear-roster ()
  "Clean up the roster."
  ;; This is made complicated by the fact that the JIDs are symbols with properties.
  (mapatoms #'(lambda (x)
		(unintern x jabber-jid-obarray))
	    jabber-jid-obarray)
  (setq *jabber-roster* nil))

(defun jabber-send-sexp (sexp)
  "send the xml corresponding to SEXP to the jabber server"
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
  (process-send-string *jabber-connection* (jabber-sexp2xml sexp)))

(provide 'jabber-core)

;;; arch-tag: 9d273ce6-c45a-447b-abf3-21d3ce73a51a
