;;; jabber-activity.el --- show jabber activity in the mode line

;; Copyright (C) 2004 Carl Henrik Lunde - <chlunde+jabber+@ping.uio.no>

;; This file is a part of jabber.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Allows tracking messages from buddys using the global mode line

;;; TODO:

;; - Disconnect hook which removes us from the global-mode-string?
;; - Add a hook which notifies the user about unread messages when she
;;   tries to close emacs

;;; Code:

(require 'jabber-alert)
(require 'jabber-util)

(defgroup jabber-activity nil
  "activity tracking options"
  :group 'jabber)

(defcustom jabber-activity-make-string 'jabber-activity-make-string-default
  "Function to call, for making the string to put in the mode
line.  The default function returns the nick of the user."
  :type 'function
  :group 'jabber-activity)

(defcustom jabber-activity-show-p 'jabber-activity-show-p-default
  "Predicate function to call to check if the given jid should be
shown in the mode line or not."
  :type 'function
  :group 'jabber-activity)

(defface jabber-activity-face
  '((t (:foreground "blue" :weight bold)))
  "The face for displaying jabber-activity-string in the mode line"
  :group 'jabber-activity)

(defvar jabber-activity-jids nil
  "A list of JIDs which have caused activity")

;; Protect this variable from beeing set in Local variables etc.
(put 'jabber-activity-mode-string 'risky-local-variable t)

(defvar jabber-activity-mode-string ""
  "The mode string for jabber activity")

(defun jabber-activity-make-string-default (jid)
  "Return the nick of the JID.  If no nick is available, return
the username part of the JID."
  (let ((nick (jabber-jid-displayname jid))
	(user (jabber-jid-user jid))
	(username (jabber-jid-username jid)))
    (if (string= nick user)
	username
      nick)))

(defun jabber-activity-show-p-default (jid)
  "Returns t only if there is no visible buffer for JID"
  (not (get-buffer-window (jabber-chat-get-buffer jid) 'visible)))

(defun jabber-activity-mode-line-update ()
  "Update the string shown in the mode line using `jabber-activity-make-string'
on JIDs where `jabber-activity-show-p'"
  (setq jabber-activity-mode-string
	(if jabber-activity-jids
	    (concat " "
		    (jabber-propertize (mapconcat jabber-activity-make-string
						  jabber-activity-jids
						  ",")
				       'face 'jabber-activity-face))
	  ""))
  (force-mode-line-update 'all))

(require 'cl)

(defun jabber-activity-clean ()
  "Remove JIDs where `jabber-activity-show-p' no longer is true"
  (setq jabber-activity-jids (delete-if-not jabber-activity-show-p jabber-activity-jids))
  (jabber-activity-mode-line-update))

(defun jabber-activity-add (from buffer text proposed-alert)
  "Add a JID to mode line when `jabber-activity-show-p'"
  (when (funcall jabber-activity-show-p from)
    (add-to-list 'jabber-activity-jids from)
    (jabber-activity-mode-line-update)))

(defun jabber-activity-echo (from buffer text proposed-alert)
  "Use `jabber-message-echo' only if `jabber-activity-show-p'"
  (when (funcall jabber-activity-show-p (symbol-name from))
    (jabber-message-echo from buffer text proposed-alert)))

;;;###autoload
(define-minor-mode jabber-activity-mode
  "Toggle display of activity in hidden jabber buffers in the mode line.

With a numeric arg, enable this display if arg is positive."
  :global t
  :group 'jabber-activity
  (if jabber-activity-mode
      (progn
	(add-hook 'window-configuration-change-hook
		  'jabber-activity-clean)
	(add-hook 'jabber-alert-message-hooks
		  'jabber-activity-add)
	(add-to-list 'global-mode-string
		     'jabber-activity-mode-string t))
    (progn
      (remove-hook 'window-configuration-change-hook
		   'jabber-activity-remove-visible)
      (remove-hook 'jabber-alert-message-hooks
		   'jabber-activity-add)
      (setq global-mode-string (delete 'jabber-activity-mode-string
				       global-mode-string)))))

(provide 'jabber-activity)

;; arch-tag: 127D7E42-356B-11D9-BE1E-000A95C2FCD0
;;; jabber-activity.el ends here.
