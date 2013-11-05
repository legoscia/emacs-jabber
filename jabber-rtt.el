;;; jabber-rtt.el --- XEP-0301: In-Band Real Time Text

;; Copyright (C) 2013  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar jabber-rtt-ewoc-node nil)
(make-variable-buffer-local 'jabber-rtt-ewoc-node)

(defvar jabber-rtt-last-seq nil)
(make-variable-buffer-local 'jabber-rtt-last-seq)

(defvar jabber-rtt-message nil)
(make-variable-buffer-local 'jabber-rtt-message)

(defvar jabber-rtt-pending-events nil)
(make-variable-buffer-local 'jabber-rtt-pending-events)

;; Add function last in chain, so a chat buffer is already created.
;;;###autoload
(add-to-list 'jabber-message-chain #'jabber-rtt-handle-message t)

;;;###autoload
(defun jabber-rtt-handle-message (jc xml-data)
  ;; We could support this for MUC as well, if useful.
  (when (and (not (jabber-muc-message-p xml-data))
	     (get-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from))))
    (with-current-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from))
      (let* ((rtt (jabber-xml-path xml-data '(("urn:xmpp:rtt:0" . "rtt"))))
	     (body (jabber-xml-path xml-data '(body)))
	     (seq (when rtt (jabber-xml-get-attribute rtt 'seq)))
	     (event (when rtt (or (jabber-xml-get-attribute rtt 'event) "edit")))
	     (actions (when rtt (jabber-xml-node-children rtt)))
	     (inhibit-read-only t))
	(cond
	 ((or body (string= event "cancel"))
	  ;; A <body/> element supersedes real time text.
	  (when jabber-rtt-ewoc-node
	    (ewoc-delete jabber-chat-ewoc jabber-rtt-ewoc-node))
	  (setq jabber-rtt-ewoc-node nil
		jabber-rtt-last-seq nil
		jabber-rtt-message nil
		jabber-rtt-pending-events nil))
	 ((member event '("new" "reset"))
	  (when jabber-rtt-ewoc-node
	    (ewoc-delete jabber-chat-ewoc jabber-rtt-ewoc-node))
	  (setq jabber-rtt-ewoc-node
		(ewoc-enter-last jabber-chat-ewoc (list :notice "[typing...]"))
		jabber-rtt-last-seq (string-to-number seq)
		jabber-rtt-message ""
		jabber-rtt-pending-events nil)
	  (jabber-rtt--process-actions actions))
	 ((string= event "edit")
	  ;; TODO: check whether this works properly in 32-bit Emacs
	  (cond
	   ((and jabber-rtt-last-seq
		 (equal (1+ jabber-rtt-last-seq)
			(string-to-number seq)))
	    ;; We are in sync.
	    (setq jabber-rtt-last-seq (string-to-number seq))
	    (jabber-rtt--process-actions actions))
	   (t
	    ;; TODO: show warning when not in sync
	    (message "out of sync! %s vs %s"
		     seq jabber-rtt-last-seq))
	  ))
	 ;; TODO: handle event="init"
	 )))))

(defun jabber-rtt--process-actions (actions)
  (dolist (action actions)
    (case (jabber-xml-node-name action)
      ((t)
       ;; insert text
       (let* ((p (jabber-xml-get-attribute action 'p))
	      (position (if p (string-to-number p) (length jabber-rtt-message))))
	 (setq position (max position 0))
	 (setq position (min position (length jabber-rtt-message)))
	 (setf (substring jabber-rtt-message position position)
	       (car (jabber-xml-node-children action)))

	 (ewoc-set-data jabber-rtt-ewoc-node (list :notice (concat "[typing...] " jabber-rtt-message)))
	 (let ((inhibit-read-only t))
	   (ewoc-invalidate jabber-chat-ewoc jabber-rtt-ewoc-node))))
      ((e)
       ;; erase text
       (let* ((p (jabber-xml-get-attribute action 'p))
	      (position (if p (string-to-number p) (length jabber-rtt-message)))
	      (n (jabber-xml-get-attribute action 'n))
	      (number (if n (string-to-number n) 1)))
	 (setq position (max position 0))
	 (setq position (min position (length jabber-rtt-message)))
	 (setq number (max number 0))
	 (setq number (min number position))
	 ;; Now erase the NUMBER characters before POSITION.
	 (setf (substring jabber-rtt-message (- position number) position)
	       "")

	 (ewoc-set-data jabber-rtt-ewoc-node (list :notice jabber-rtt-message))
	 (let ((inhibit-read-only t))
	   (ewoc-invalidate jabber-chat-ewoc jabber-rtt-ewoc-node))))
      ((w)
       ;; TODO: handle <w/>
       ))))

(provide 'jabber-rtt)
;;; jabber-rtt.el ends here
