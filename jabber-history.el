;; jabber-history.el - recording message history

;; Copyright (C) 2004 - Mathias Dahl
;; Copyright (C) 2004 - Magnus Henoch - mange@freemail.hu

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


;;
;; Changes
;;
;; 2004-11-22, Mathias Dahl

;;  Changed "(jabber-jid-displayname from)" to "from" as it is more
;;  "raw"

;;  Added new logging function jabber-message-log-log-message

;;  Changed (temporarily) jabber-send-chat to log outgoing messages
;;  too

;;  Now encoding \n and \r in message to keep it on one line

;;  Added "direction" data to the log

;;  Using format-time-string to get ISO 8601 date and time format.
;;  See http://www.cl.cam.ac.uk/~mgk25/iso-time.html

(defcustom jabber-history-enabled nil
  "Non-nil means message logging is enabled"
  :type 'boolean
  :group 'jabber)

(defun jabber-message-history (from buffer text proposed-alert)
  "Log message to log file. For now, all messages from all users
will be logged to the same file."
  (if jabber-history-enabled
      (jabber-history-log-message "in" from nil text)))

(defun jabber-history-log-message (direction from to body)
  "Log a message"
  (with-temp-buffer
    ;; Encode text as Lisp string - get decoding for free
    (setq body (prin1-to-string body))
    ;; Encode LF and CR
    (while (string-match "\n" body)
      (setq body (replace-match "\\n" nil t body nil)))
    (while (string-match "\r" body)
      (setq body (replace-match "\\r" nil t body nil)))
    (insert (format "%s: %s: %s: %s: %s\n"
		    (format-time-string "%Y-%m-%d %T %z")
		    (or direction
			"in")
		    (or from
			"me")
		    (or to
			"me")
		    body))
    (append-to-file (point-min) (point-max) "~/.jabber_global_message_log")))

;; Changed version of jabber-send-chat just to test concept. I don't
;; know if placing the message loggin for outgoing messages here is
;; the best place.

;; (defun jabber-send-chat (to body)
;;   "send a chat message to someone"
;;   (jabber-send-message to nil body "chat")
;;   (if jabber-history-enabled
;;       (jabber-history-log-message "out" nil to body)))

;; Try it with:
;; (setq jabber-history-enabled t)
;; (add-hook 'jabber-alert-message-hooks 'jabber-message-history)

(provide 'jabber-history)

;; arch-tag: 0AA0C235-3FC0-11D9-9FE7-000A95C2FCD0
