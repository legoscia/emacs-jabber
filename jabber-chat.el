;; jabber-chat.el - chat buffer display, basic groupchat functions

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

(require 'jabber-core)
(require 'jabber-keymap)
(require 'jabber-util)

(require 'format-spec)

(defgroup jabber-chat nil "chat display options"
  :group 'jabber)

(defcustom jabber-chat-buffer-format "*-jabber-chat-%n-*"
  "The format specification for the name of chat buffers.

These fields are available (all are about the person you are chatting
with):

%n   Nickname, or JID if no nickname set
%j   Bare JID (without resource)
%r   Resource"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-header-line-format '(" " (:eval (jabber-jid-displayname jabber-chatting-with))
					    "\t" (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
							  (propertize 
							   (or
							    (cdr (assoc (get buddy 'show) jabber-presence-strings))
							    (get buddy 'show))
							   'face
							   (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
							       'jabber-roster-user-online))))
					    "\t" (:eval (get (jabber-jid-symbol jabber-chatting-with) 'status)))
  "The specification for the header line of chat buffers.

The format is that of `mode-line-format' and `header-line-format'."
  :type 'sexp
  :group 'jabber-chat)

(defcustom jabber-groupchat-buffer-format "*-jabber-groupchat-%n-*"
  "The format specification for the name of groupchat buffers.

These fields are available (all are about the person you are chatting
with):

%n   Roster name of group, or JID if no nickname set
%j   Bare JID (without resource)"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-time-format "%H:%M"
  "The format specification for time displayed in the chat buffer.

See `format-time-string' for valid values."
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-local-prompt-format "[%t] %n> "
  "The format specification for lines you type in the chat buffer.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
%n   Nickname (`jabber-nickname')
%u   Username
%r   Resource
%j   Bare JID (without resource)"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-foreign-prompt-format "[%t] %n> "
  "The format specification for lines others type in the chat buffer.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
%n   Nickname, or JID if no nickname set
%u   Username
%r   Resource
%j   Bare JID (without resource)"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-system-prompt-format "[%t] *** "
  "The format specification for lines from the system or that are special in the chat buffer."
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-groupchat-prompt-format "[%t] %n> "
  "The format specification for lines in groupchat.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
%n, %u, %r
     Nickname in groupchat
%j   Full JID (room@server/nick)"
  :type 'string
  :group 'jabber-chat)

(defface jabber-chat-prompt-local
  '((t (:foreground "blue" :weight bold)))
  "face for displaying the chat prompt for what you type in"
  :group 'jabber-chat)

(defface jabber-chat-prompt-foreign
  '((t (:foreground "red" :weight bold)))
  "face for displaying the chat prompt for what they send"
  :group 'jabber-chat)

(defface jabber-chat-prompt-system
  '((t (:foreground "green" :weight bold)))
  "face used for system and special messages"
  :group 'jabber-chat)

(defvar jabber-chatting-with nil
  "JID of the person you are chatting with")

(defvar jabber-group nil
  "the groupchat you are participating in")

(defvar *jabber-active-groupchats* nil
  "alist of groupchats and nicknames")

(defun jabber-chat-mode ()
  "\\{jabber-chat-mode-map}"
  (kill-all-local-variables)
  (make-local-variable 'jabber-chatting-with)

  (make-local-variable 'scroll-conservatively)
  (setq scroll-conservatively 5)

  (make-local-variable 'jabber-point-insert)
  (setq jabber-point-insert (point-min))

  (setq header-line-format jabber-chat-header-line-format)

  (setq major-mode 'jabber-chat-mode
        mode-name "jabber-chat")
  (use-local-map jabber-chat-mode-map))

(put 'jabber-chat-mode 'mode-class 'special)

(defvar jabber-chat-mode-map (copy-keymap jabber-common-keymap))

(define-key jabber-chat-mode-map "\r" 'jabber-chat-buffer-send)

(defun jabber-replace-me (body nick)
  "Replaces /me with NICK if it occurs at the beginning of BODY"
  (replace-regexp-in-string "^/me" nick body))

(defun jabber-format-prompt (prompt time n u r j)
  (format-spec prompt
	       (list
		(cons ?t time)
		(cons ?n n)
		(cons ?u u)
		(cons ?r r)
		(cons ?j j))))

(defun jabber-format-body (body prompt face time nick user resource jid)
  "Format a string for a chat buffer according to user's preferences.
BODY is the text to format.
PROMPT is a format string for the prompt, like
`jabber-chat-local-prompt-format'.
FACE is the face to use for the prompt.
TIME is the time to present, as a string.
NICK is the nickname.
USER is the username (usually the username portion of a JID).
RESOURCE is the resource.
JID is the bare JID."
  (if (and (>= (length body) 3)
	   (string-equal (substring body 0 3) "/me"))
      (concat
       (jabber-propertize (jabber-format-prompt prompt
						time
						""
						user
						resource
						jid)
			  'face face)
       (jabber-propertize (concat (jabber-replace-me body jabber-nickname)
				  "\n")
			  'face 'jabber-chat-prompt-system))
    (concat (jabber-propertize (jabber-format-prompt prompt
						     time
						     nick
						     user
						     resource
						     jid)
			       'face face)
	    body "\n")))

(defun jabber-chat-buffer-send ()
  (interactive)
  (let ((body (delete-and-extract-region jabber-point-insert (point-max))))
    ;; If user accidentally hits RET without writing anything,
    ;; delete-and-extract-region returns nil.  In that case,
    ;; no message should be sent.
    (when body
      (jabber-send-chat jabber-chatting-with body)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
	(insert (jabber-format-body body
				    jabber-chat-local-prompt-format
				    'jabber-chat-prompt-local
				    (format-time-string jabber-chat-time-format)
				    jabber-nickname
				    jabber-username
				    jabber-resource
				    (concat jabber-username "@" jabber-server)))
					       
	(setq jabber-point-insert (point))
	(set-text-properties jabber-point-insert (point-max) nil)
	(put-text-property (point-min) (point-max) 'read-only t)
	(put-text-property (point-min) (point-max) 'front-sticky t)
	(put-text-property (point-min) (point-max) 'rear-nonsticky t)))))

(defun jabber-chat-get-buffer (chat-with)
  "Return the chat buffer for chatting with CHAT-WITH (bare or full JID).
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'."
  (format-spec jabber-chat-buffer-format
	       (list
		(cons ?n (jabber-jid-displayname chat-with))
		(cons ?j (jabber-jid-user chat-with))
		(cons ?r (jabber-jid-resource chat-with)))))

(defun jabber-chat-create-buffer (chat-with)
  "Prepare a buffer for chatting with CHAT-WITH.
This function is idempotent."
  (with-current-buffer (get-buffer-create (jabber-chat-get-buffer chat-with))
    (if (not (eq major-mode 'jabber-chat-mode)) (jabber-chat-mode))
    (setq jabber-chatting-with chat-with)
    (current-buffer)))

(defun jabber-chat-display (from body &optional timestamp)
  "display the chat window and a new message, if there is one.
TIMESTAMP is timestamp, or nil for now."
  (with-current-buffer (jabber-chat-create-buffer from)
    (goto-char jabber-point-insert)

    (let ((inhibit-read-only t))
      (if body (insert (jabber-format-body body
					   jabber-chat-foreign-prompt-format
					   'jabber-chat-prompt-foreign
					   (format-time-string jabber-chat-time-format timestamp)
					   (jabber-jid-displayname from)
					   (jabber-jid-username from)
					   (jabber-jid-resource from)
					   (jabber-jid-user from))))
      (setq jabber-point-insert (point))
      (set-text-properties jabber-point-insert (point-max) nil)
      (put-text-property (point-min) jabber-point-insert 'read-only t)
      (put-text-property (point-min) jabber-point-insert 'front-sticky t)
      (put-text-property (point-min) jabber-point-insert 'rear-nonsticky t))
 
    (goto-char (point-max))

    (run-hook-with-args 'jabber-alert-message-hooks from (current-buffer) body (funcall jabber-alert-message-function from (current-buffer) body))))

(add-to-list 'jabber-jid-chat-menu
	     (cons "Send message" 'jabber-send-message))

(defun jabber-send-message (to subject body type)
  "send a message tag to the server"
  (interactive (list (jabber-read-jid-completing "to: ")
		     (jabber-read-with-input-method "subject: ")
		     (jabber-read-with-input-method "body: ")
		     (read-string "type: ")))
  (jabber-send-sexp `(message ((to . ,to)
                               ,(if (> (length type) 0)
                                    `(type . ,type)))
                              ,(if (> (length subject) 0)
                                   `(subject () ,(jabber-escape-xml subject)))
                              ,(if (> (length body) 0)
                                   `(body () ,(jabber-escape-xml body))))))

(add-to-list 'jabber-jid-chat-menu
	     (cons "Start chat" 'jabber-chat-with))

(defun jabber-chat-with (jid)
  "open an empty chat window for chatting with JID"
  (interactive (list (jabber-read-jid-completing "chat with:")))
  (switch-to-buffer (jabber-chat-create-buffer jid)))

(defun jabber-chat-with-jid-at-point ()
  "Start chat with JID at point.
Signal an error if there is no JID at point."
  (interactive)
  (let ((jid-at-point (get-text-property (point)
					 'jabber-jid)))
    (if jid-at-point
	(jabber-chat-with jid-at-point)
      (error "No contact at point"))))

(defun jabber-groupchat-mode ()
  "\\{jabber-groupchat-mode-map}"
  (kill-all-local-variables)
  (make-local-variable 'jabber-group)
  (make-local-variable 'scroll-conservatively)
  (setq scroll-conservatively 5)
  (make-local-variable 'jabber-point-insert)
  (setq jabber-point-insert (point-min))
  (setq major-mode 'jabber-groupchat-mode
        mode-name "jabber-groupchat")
  (use-local-map jabber-groupchat-mode-map))

(put 'jabber-groupchat-mode 'mode-class 'special)

(defvar jabber-groupchat-mode-map (copy-keymap jabber-common-keymap))

(define-key jabber-groupchat-mode-map "\r" 'jabber-groupchat-buffer-send)

(defun jabber-groupchat-buffer-send ()
  (interactive)
  (let ((body (delete-and-extract-region jabber-point-insert (point-max)))
	(inhibit-read-only t))
    (jabber-send-groupchat jabber-group body)
    (goto-char (point-max))
    (setq jabber-point-insert (point-max))
    (set-text-properties jabber-point-insert (point-max) nil)
    (put-text-property (point-min) (point-max) 'read-only t)
    (put-text-property (point-min) (point-max) 'front-sticky t)
    (put-text-property (point-min) (point-max) 'rear-nonsticky t)))

(defun jabber-groupchat-get-buffer (group)
  "Return the chat buffer for chatting with CHAT-WITH (bare or full JID).
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'."
  (format-spec jabber-groupchat-buffer-format
	       (list
		(cons ?n (jabber-jid-displayname group))
		(cons ?j (jabber-jid-user group)))))

(defun jabber-groupchat-create-buffer (group)
  "Prepare a buffer for groupchat in GROUP.
This function is idempotent."
  (with-current-buffer (get-buffer-create (jabber-groupchat-get-buffer group))
    (if (not (eq major-mode 'jabber-groupchat-mode)) (jabber-groupchat-mode))
    (setq jabber-group group)
    (current-buffer)))

(defun jabber-groupchat-display (group &optional nick body timestamp)
  "display the chat window and a new message, if there is one.
TIMESTAMP is timestamp, or nil for now."
  (with-current-buffer (jabber-groupchat-create-buffer group)
    (goto-char jabber-point-insert)
    (let ((inhibit-read-only t))
      (if body (insert (jabber-format-body body
					   jabber-groupchat-prompt-format
					   'jabber-chat-prompt-foreign
					   (format-time-string jabber-chat-time-format timestamp)
					   (or nick "")
					   (or nick "")
					   (or nick "")
					   (concat group "/" nick))))
      (setq jabber-point-insert (point))
      (set-text-properties jabber-point-insert (point-max) nil)
      (put-text-property (point-min) jabber-point-insert 'read-only t)
      (put-text-property (point-min) jabber-point-insert 'front-sticky t)
      (put-text-property (point-min) jabber-point-insert 'rear-nonsticky t))
 
    (goto-char (point-max))

    (setq jabber-group group)
    (run-hook-with-args 'jabber-alert-message-hooks nick (current-buffer) body (funcall jabber-alert-message-function group (current-buffer) body))))

(add-to-list 'jabber-jid-muc-menu
	     (cons "Leave groupchat" 'jabber-groupchat-leave))

(defun jabber-groupchat-leave (group)
  "leave a groupchat"
  (interactive (list (completing-read (format "Leave which group: %s" (if jabber-group (concat "(default: " jabber-group ") ")))
				      *jabber-active-groupchats*
				      nil nil nil nil
				      jabber-group)))
  (let ((whichgroup (assoc group *jabber-active-groupchats*)))
    (setq *jabber-active-groupchats* 
	  (delq whichgroup *jabber-active-groupchats*))

    ;; send unavailable presence to our own nick in room
    (jabber-send-sexp `(presence ((to . ,(format "%s/%s" group (cdr whichgroup)))
				  (type . "unavailable"))))))

(add-to-list 'jabber-jid-muc-menu
	     (cons "Join groupchat" 'jabber-groupchat-join))

(defun jabber-groupchat-join (group nickname)
  "join a groupchat"
  (interactive (list (jabber-read-jid-completing "group: ")
		     (jabber-read-with-input-method (format "Nickname: (default %s) "
							    jabber-nickname) 
						    nil nil jabber-nickname)))
  (jabber-send-sexp `(presence ((to . ,(format "%s/%s" group nickname)))))

  (let ((whichgroup (assoc group *jabber-active-groupchats*)))
    (if whichgroup
	(setcdr whichgroup nickname)
      (add-to-list '*jabber-active-groupchats* (cons group nickname))))
  
  (jabber-groupchat-display group))

(add-to-list 'jabber-message-chain 'jabber-process-message)

(defun jabber-process-message (xml-data)
  "process incoming messages"
  (let ((from (jabber-xml-get-attribute xml-data 'from))
	(type (jabber-xml-get-attribute xml-data 'type))
	(subject (car (xml-node-children (car (jabber-xml-get-children xml-data 'subject)))))
	(body (car (xml-node-children (car (jabber-xml-get-children xml-data 'body)))))
	(thread (car (xml-node-children (car (jabber-xml-get-children xml-data 'thread)))))
	(timestamp (car (delq nil (mapcar 'jabber-x-delay (jabber-xml-get-children xml-data 'x)))))
	(error (car (jabber-xml-get-children xml-data 'error))))

    (cond
     ;; Public groupchat messages have type "groupchat" and are from room@server/nick.
     ;; Public groupchat errors have type "error" and are from room@server.
     ((or 
       (string= type "groupchat")
       (and (string= type "error")
	    (assoc from *jabber-active-groupchats*)))

      (jabber-groupchat-display (jabber-jid-user from) 
				(jabber-jid-resource from)
				(if error
				    (concat "ERROR: " (jabber-parse-error error))
				  (jabber-unescape-xml body))
				timestamp))
     ;; Here go normal one-to-one messages and private groupchat messages.
     (t
      ;; If there is no body, we can't display it (yet), so ignore the message.
      ;; Error messages should not be ignored.
      (when (or body (string= type "error"))
	(jabber-chat-display from 
			     (if error
				 (concat "ERROR: " (jabber-parse-error error))
			       (jabber-unescape-xml body))
			     timestamp))))))

(defun jabber-send-groupchat (group body)
  "send a message to a groupchat"
  (jabber-send-message group nil body "groupchat"))

(defun jabber-send-chat (to body)
  "send a chat message to someone"
  (jabber-send-message to nil body "chat"))

(provide 'jabber-chat)

;;; arch-tag: a6cca037-2fcd-4e3b-8a40-d00523aebff5
