;; jabber-chat.el - chat buffer display, basic groupchat functions
;; $Id: jabber-chat.el,v 1.4 2004/03/29 20:07:52 legoscia Exp $

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
(require 'jabber-roster)		;for keymap
(require 'jabber-util)

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
  (setq major-mode 'jabber-chat-mode
        mode-name "jabber-chat")
  (use-local-map jabber-chat-mode-map))

(put 'jabber-chat-mode 'mode-class 'special)

(defvar jabber-chat-mode-map (copy-keymap jabber-roster-mode-map))

(dolist (key (append "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890`~!@#$%^&*()_+-=[]{}|';:/?.,>< " nil))
  (let ((send-fun (make-symbol (concat "jabber-chat-buffer-send-" (char-to-string key)))))
    (fset send-fun `(lambda (body) (interactive (list (jabber-read-with-input-method "" ,(char-to-string key))))
		      (jabber-send-chat jabber-chatting-with body)
		      (setq buffer-read-only nil)
		      (goto-char (point-max))
		      (insert (jabber-propertize (concat "[" (substring (current-time-string) 11 16) "] " jabber-username)
                                          'face 'jabber-chat-prompt-local) "> " body "\n")
		      (setq buffer-read-only t)))
    (define-key jabber-chat-mode-map (char-to-string key) send-fun)))

(defun jabber-chat-display (from body &optional timestamp)
  "display the chat window and a new message, if there is one.
TIMESTAMP is timestamp, or nil for now."
  (with-current-buffer (get-buffer-create (concat "*-jabber-chat-:-" (jabber-jid-displayname from) "-*"))
    (goto-char (point-max))

    (let ((inhibit-read-only t))
      (if body (insert (jabber-propertize (concat "[" (substring (current-time-string timestamp) 11 16) "] " (jabber-jid-displayname from))
					  'face 'jabber-chat-prompt-foreign)
		       "> " body "\n")))

    ;; Setting the major mode more than once will wipe out buffer-local
    ;; variables, therefore caution.
    (if (not (eq major-mode 'jabber-chat-mode))
	(jabber-chat-mode))

    ;; The following means that whenever you receive a message from the
    ;; person you are chatting with, the resource to which messages from
    ;; the current chat buffer will be sent is updated.  This may or may
    ;; not be what you want.
    (if from
	(setq jabber-chatting-with from))
    (run-hook-with-args 'jabber-alert-message-hooks from (current-buffer) body (funcall jabber-alert-message-function from (current-buffer) body))))

(add-to-list 'jabber-jid-chat-menu
	     (cons "Send message" 'jabber-send-message))
(defun jabber-send-message (to body subject type)
  "send a message tag to the server"
  (interactive (list (jabber-read-jid-completing "to: ")
		     (jabber-read-with-input-method "body: ")
		     (jabber-read-with-input-method "subject: ")
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
  (jabber-chat-display jid nil))

(defun jabber-groupchat-mode ()
  "\\{jabber-groupchat-mode-map}"
  (kill-all-local-variables)
  (make-local-variable 'jabber-group)
  (setq major-mode 'jabber-groupchat-mode
        mode-name "jabber-groupchat")
  (use-local-map jabber-groupchat-mode-map)
  (setq buffer-read-only t))

(put 'jabber-groupchat-mode 'mode-class 'special)

(defvar jabber-groupchat-mode-map (copy-keymap jabber-roster-mode-map))

(dolist (key (append "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890`~!@#$%^&*()_+-=[]{}|';:/?.,>< " nil))
  (let ((send-fun (make-symbol (concat "jabber-groupchat-buffer-send-" (char-to-string key)))))
    (fset send-fun `(lambda (body) (interactive (list (jabber-read-with-input-method "" ,(char-to-string key))))
		      (jabber-send-groupchat jabber-group body)
		      (setq buffer-read-only nil)
		      (goto-char (point-max))
		      (if (not (eq major-mode 'jabber-groupchat-mode))
			  (jabber-groupchat-mode))))
    (define-key jabber-groupchat-mode-map (char-to-string key) send-fun)))

(defun jabber-groupchat-display (group &optional nick body timestamp)
  "display the groupchat window and an incoming message, if there is one.
TIMESTAMP is timestamp, or nil for now."
  (with-current-buffer (get-buffer-create (concat "*-jabber-groupchat-:-" group "-*"))
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (if body (insert (jabber-propertize (concat "[" (substring (current-time-string timestamp) 11 16) "] " nick)
                                 'face 'jabber-chat-prompt-foreign)
                     "> " body "\n"))
    (if (not (eq major-mode 'jabber-groupchat-mode))
	(jabber-groupchat-mode))
    (setq jabber-group group)
    (run-hook-with-args 'jabber-alert-message-hooks group (current-buffer) body (funcall jabber-alert-message-function group (current-buffer) body))))

(add-to-list 'jabber-jid-muc-menu
	     (cons "Leave groupchat" 'jabber-groupchat-leave))
(defun jabber-groupchat-leave (group)
  "leave a groupchat"
  (interactive (list (completing-read "group: "
				      *jabber-active-groupchats*)))
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
	(error (car (xml-get-children xml-data 'error))))

    (cond
     ;; Public groupchat messages have type "groupchat" and are from room@server/nick.
     ;; Public groupchat errors have type "error" and are from room@server.
     ((or 
       (and (string= type "groupchat")
	    (assoc (jabber-jid-user from) *jabber-active-groupchats*))
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
      (jabber-chat-display from 
			   (if error
			       (concat "ERROR: " (jabber-parse-error error))
			     (jabber-unescape-xml body))
			   timestamp)))))

(defun jabber-send-groupchat (group body)
  "send a message to a groupchat"
  (jabber-send-message group body nil "groupchat"))

(defun jabber-send-chat (to body)
  "send a chat message to someone"
  (jabber-send-message to body nil "chat"))

(provide 'jabber-chat)
