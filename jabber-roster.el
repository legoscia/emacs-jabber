;; jabber-roster.el - displaying the roster

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

(require 'jabber-presence)
(require 'jabber-util)
(require 'jabber-alert)
(require 'jabber-keymap)

(defgroup jabber-roster nil "roster display options"
  :group 'jabber)

(defcustom jabber-roster-line-spacing 0
  "Number of empty lines between roster items"
  :type 'integer
  :group 'jabber-roster)

(defcustom jabber-show-resources 'sometimes
  "Show resources in roster?"
  :type '(radio (const :tag "Never" nil)
		(const :tag "When more than one connected resource" sometimes)
		(const :tag "Always" always))
  :group 'jabber-roster)

(defface jabber-roster-user-online
  '((t (:foreground "blue" :weight bold :slant normal)))
  "face for displaying online users"
  :group 'jabber-roster)

(defface jabber-roster-user-xa
  '((t (:foreground "black" :weight normal :slant italic)))
  "face for displaying extended away users"
  :group 'jabber-roster)

(defface jabber-roster-user-dnd
  '((t (:foreground "red" :weight normal :slant italic)))
  "face for displaying do not disturb users"
  :group 'jabber-roster)

(defface jabber-roster-user-away
  '((t (:foreground "dark green" :weight normal :slant italic)))
  "face for displaying away users"
  :group 'jabber-roster)

(defface jabber-roster-user-chatty
  '((t (:foreground "dark orange" :weight bold :slant normal)))
  "face for displaying chatty users"
  :group 'jabber-roster)

(defface jabber-roster-user-error
  '((t (:foreground "red" :weight light :slant italic)))
  "face for displaying users sending presence errors"
  :group 'jabber-roster)

(defface jabber-roster-user-offline
  '((t (:foreground "dark grey" :weight light :slant italic)))
  "face for displaying offline users"
  :group 'jabber-roster)

(defvar jabber-roster-mode-map (copy-keymap jabber-common-keymap))

(defun jabber-roster-mode ()
  "Major mode for Jabber roster display.
Use the keybindings (mnemonic as Chat, Roster, Info, MUC, Service) to
bring up menus of actions.
\\{jabber-roster-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'jabber-roster-mode
	mode-name "jabber-roster")
  (use-local-map jabber-roster-mode-map)
  (setq buffer-read-only t))

(put 'jabber-roster-mode 'mode-class 'special)

(defun jabber-sort-roster ()
  "sort roster according to online status"
  (setq *jabber-roster*
	(sort *jabber-roster*
	      #'(lambda (a b)
		  (let ((a-show (get a 'show))
			(b-show (get b 'show)))
		    (> (length (member a-show jabber-sort-order))
		       (length (member b-show jabber-sort-order))))))))

(defun jabber-display-roster ()
  "switch to the main jabber buffer and refresh the roster display to reflect the current information"
  (interactive)
  (with-current-buffer (process-buffer *jabber-connection*)
    (if (not (eq major-mode 'jabber-roster-mode))
	(jabber-roster-mode))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (jabber-propertize jabber-server 'face 'jabber-title-large) "\n__________________________________\n\n")
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-2] #'jabber-send-presence)
      (insert (jabber-propertize (concat (format " - %s"
						 (cdr (assoc *jabber-current-show* jabber-presence-strings)))
					 (if (not (zerop (length *jabber-current-status*)))
					     (format " (%s)"
						     *jabber-current-status*))
					 " -")
				 'face (or (cdr (assoc *jabber-current-show* jabber-presence-faces))
					   'jabber-roster-user-online)
				 ;;'mouse-face (cons 'background-color "light grey")
				 'keymap map)
              "\n__________________________________\n\n"))

    (jabber-sort-roster)
    (dolist (buddy *jabber-roster*)
      (let ((buddy-str (concat (if (get buddy 'connected)
				   " * "
				 "   ")
			       (if (> (length (get buddy 'name)) 0)
				   (get buddy 'name)
				 (symbol-name buddy))
			       (format " - %s" (or
						(cdr (assoc (get buddy 'show) jabber-presence-strings))
						(get buddy 'show)))
			       (if (get buddy 'status)
				   (format " (%s)" (get buddy 'status)))
			       (if jabber-debug-roster
				   (format " --- [%S] ---" (symbol-plist buddy)))
			       )))
	(add-text-properties 0
			   (length buddy-str)
			   (list
			    'face
			    (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
				'jabber-roster-user-online)
			    ;;'mouse-face
			    ;;(cons 'background-color "light grey")
			    'help-echo
			    (symbol-name buddy)
			    'jabber-jid
			    (symbol-name buddy))
			   buddy-str)
	;; (let ((map (make-sparse-keymap))
;; 	      (chat-with-func (make-symbol (concat "jabber-chat-with" (symbol-name buddy)))))
;; 	  (fset chat-with-func `(lambda () (interactive) (jabber-chat-with ,(symbol-name buddy))))
;; 	  (define-key map [mouse-2] chat-with-func)
;; 	  (put-text-property 0
;; 			     (length buddy-str)
;; 			     'keymap 
;; 			     map
;; 			     buddy-str))
	(insert buddy-str "\n"))

	(when (or (eq jabber-show-resources 'always)
		  (and (eq jabber-show-resources 'sometimes)
		       (> (jabber-count-connected-resources buddy) 1)))
	  (dolist (resource (get buddy 'resources))
	    (when (plist-get (cdr resource) 'connected)
	      (let ((resource-str (concat "     "
					  (if (> (length (car resource)) 0)
					      (car resource)
					    "empty")
					  (format " - %s" (or
							   (cdr (assoc (plist-get (cdr resource) 'show) jabber-presence-strings))
							   (plist-get (cdr resource) 'show)))
					  (if (plist-get (cdr resource) 'status)
					      (format " (%s)" (plist-get (cdr resource) 'status)))
					  (format ", priority %d" (plist-get (cdr resource) 'priority)))))
		(add-text-properties 0
				     (length resource-str)
				     (list
				      'face
				      (or (cdr (assoc (plist-get (cdr resource) 'show) jabber-presence-faces))
					  'jabber-roster-user-online)
				      'jabber-jid
				      (format "%s/%s" (symbol-name buddy) (car resource)))
				     resource-str)
		(insert resource-str "\n")))))
	(insert (make-string jabber-roster-line-spacing ?\n)))
    (insert "__________________________________")
    (goto-char (point-min))
    (setq buffer-read-only t)
    (if (interactive-p)
	(run-hook-with-args 'jabber-alert-info-message-hooks 'roster (current-buffer) (funcall jabber-alert-info-message-function 'roster (current-buffer))))))

(provide 'jabber-roster)

;;; arch-tag: 096af063-0526-4dd2-90fd-bc6b5ba07d32
