;; jabber-keepalive.el - try to detect lost connection

;; Copyright (C) 2007 - Detlev Zundel - dzu@gnu.org
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

;;; These keepalive functions send a jabber:iq:time request to the
;;; server every X minutes, and considers the connection broken if
;;; they get no answer within Y seconds.

(defgroup jabber-keepalive nil
  "Keepalive functions try to detect lost connection"
  :group 'jabber)

(defcustom jabber-keepalive-interval 600
  "Interval in seconds between connection checks."
  :type 'integer
  :group 'jabber-keepalive)

(defcustom jabber-keepalive-timeout 20
  "Seconds to wait for response from server."
  :type 'integer
  :group 'jabber-keepalive)

(defvar jabber-keepalive-timer nil
  "Timer object for keepalive function")

(defvar jabber-keepalive-timeout-timer nil
  "Timer object for keepalive timeout function")

(defvar jabber-keepalive-pending nil
  "List of outstanding keepalive connections")

(defvar jabber-keepalive-debug nil
  "Log keepalive traffic when non-nil")

(defun jabber-keepalive-start (&optional jc)
  "Activate keepalive.
The JC argument makes it possible to add this function to
`jabber-post-connect-hooks'; it is ignored.  Keepalive is activated
for all accounts regardless of the argument."
  (interactive)

  (when jabber-keepalive-timer
    (jabber-keepalive-stop))

  (setq jabber-keepalive-timer
	(run-with-timer 5
			jabber-keepalive-interval
			'jabber-keepalive-do))
  (add-hook 'jabber-post-disconnect-hook 'jabber-keepalive-stop))

(defun jabber-keepalive-stop ()
  "Deactivate keepalive"
  (interactive)

  (when jabber-keepalive-timer
    (jabber-cancel-timer jabber-keepalive-timer)
    (setq jabber-keepalive-timer nil)))

(defun jabber-keepalive-do ()
  (when jabber-keepalive-debug
    (message "%s: sending keepalive packet(s)" (current-time-string)))
  (setq jabber-keepalive-timeout-timer
	(run-with-timer jabber-keepalive-timeout
			nil
			'jabber-keepalive-timeout))
  (setq jabber-keepalive-pending jabber-connections)
  (dolist (c jabber-connections)
    ;; Whether we get an error or not is not interesting.
    ;; Getting a response at all is.
    (jabber-send-iq c nil "get"
		    '(query ((xmlns . "jabber:iq:time")))
		    'jabber-keepalive-got-response nil
		    'jabber-keepalive-got-response nil)))

(defun jabber-keepalive-got-response (jc &rest args)
  (when jabber-keepalive-debug
    (message "%s: got keepalive response from %s"
	     (current-time-string)
	     (plist-get (fsm-get-state-data jc) :server)))
  (setq jabber-keepalive-pending (remq jc jabber-keepalive-pending))
  (when (null jabber-keepalive-pending)
    (jabber-cancel-timer jabber-keepalive-timeout-timer)
    (setq jabber-keepalive-timeout-timer nil)))

(defun jabber-keepalive-timeout ()
  (jabber-cancel-timer jabber-keepalive-timer)
  (setq jabber-keepalive-timer nil)

  (dolist (c jabber-keepalive-pending)
    (message "%s: keepalive timeout, connection to %s considered lost"
	     (current-time-string)
	     (plist-get (fsm-get-state-data c) :server))

    (run-hooks jabber-lost-connection-hook)
    (jabber-disconnect-one c nil)))

(provide 'jabber-keepalive)

;;; arch-tag: d19ca743-75a1-475f-9217-83bd18012146
