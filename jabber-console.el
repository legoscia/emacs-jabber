;; jabber-console.el - XML Console mode

;; Copyright (C) 2009 - Demyan Rogozhin <demyan.rogozhin@gmail.com>

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

;;; Commentary:

;; Use *-jabber-console-* for sending custom XMPP code. Be careful!

;;; Code:

(require 'jabber-keymap)
(require 'jabber-core)
(require 'ewoc)

(defcustom jabber-console nil
  "Use XML Console for debuging, sending raw XMPP code"
  :type 'boolean
  :group 'jabber-debug)

(defvar jabber-point-insert nil
  "Position where the message being composed starts")

(defvar jabber-send-function nil
  "Function for sending a message from a chat buffer.")

(defvar jabber-console-mode-hook nil
  "Hook called at the end of `jabber-console-mode'.
Note that functions in this hook have no way of knowing
what kind of chat buffer is being created.")

(defvar jabber-console-ewoc nil
  "The ewoc showing the XML elements of this stream buffer.")

;;;###autoload
(defvar jabber-buffer-connection nil
  "The connection used by this buffer.")
;;;###autoload
(make-variable-buffer-local 'jabber-buffer-connection)

(defun jabber-console-create-buffer (jc)
  (with-current-buffer
	  (get-buffer-create (format "*-jabber-console-%s-*" (jabber-connection-bare-jid jc)))
    (unless (eq major-mode 'jabber-console-mode)
      (jabber-console-mode jc #'jabber-console-pp))
    ;; Make sure the connection variable is up to date.
    (setq jabber-buffer-connection jc)
    (current-buffer)))

(defun jabber-console-send (jc data)
  ;; Put manual string into buffers ewoc
  (jabber-process-console jc data)
  ;; ...than sent it to server
  (jabber-send-string jc data))

(defun jabber-console-pp (data)
  (cond ((stringp data)
		 ;; Handle manually entered commands
		 (insert data))
		;; Print replays from `jabber-log-xml'
		(t (insert (jabber-sexp2xml data)))))

(defun jabber-console-mode (jc ewoc-pp)
  "\\{jabber-console-mode-map}"
  (kill-all-local-variables)
  ;; Make sure to set this variable somewhere
  (make-local-variable 'jabber-send-function)
  (setq jabber-send-function 'jabber-console-send)
  (setq jabber-buffer-connection jc)

  (make-local-variable 'scroll-conservatively)
  (setq scroll-conservatively 5)

  (make-local-variable 'jabber-point-insert)
  (make-local-variable 'jabber-console-ewoc)
  (unless jabber-console-ewoc
    (setq jabber-console-ewoc
	  (ewoc-create ewoc-pp nil "<!-- + -->"))
    (goto-char (point-max))
    (put-text-property (point-min) (point) 'read-only t)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point) 'front-sticky t)
      (put-text-property (point-min) (point) 'rear-nonsticky t))
    (setq jabber-point-insert (point-marker)))

  (setq major-mode 'jabber-console-mode
        mode-name "jabber-console")
  (use-local-map jabber-console-mode-map))

(put 'jabber-console-mode 'mode-class 'special)

(defvar jabber-console-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jabber-common-keymap)
    (define-key map "\r" 'jabber-chat-buffer-send)
    map))

(defun jabber-process-console (jc xml-data)
	(with-current-buffer
		(get-buffer-create (jabber-console-create-buffer jc))
	  (let ((node
			 (ewoc-enter-last jabber-console-ewoc xml-data))))))

(provide 'jabber-console)
;;; jabber-console.el ends here
