;; jabber-chat.el - chat buffer display
;; $Id: jabber-chat.el,v 1.1 2004/02/25 21:42:02 legoscia Exp $

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

(defun jabber-chat-with (jid)
  "open an empty chat window for chatting with JID"
  (interactive (list (jabber-read-jid-completing "chat with:")))
  (jabber-chat-display jid nil))

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
    ;; XXX: The present division by type does not properly handle
    ;; groupchat error messages.
    (cond
     ((string= type "groupchat")
      (jabber-groupchat-display (jabber-jid-user from) 
				(jabber-jid-resource from)
				(jabber-unescape-xml body)
				timestamp)
      )
     (t
      (if error
	  (jabber-chat-display from
			       (concat "ERROR: "
				       (jabber-parse-error error)))
	(jabber-chat-display from 
			     (jabber-unescape-xml body)
			     timestamp))))))

(provide 'jabber-chat)
