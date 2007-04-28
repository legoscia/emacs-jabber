;; jabber-clean.el - cleanup top lines in chatbuffers

;; Copyright (C) 2007 - Kirill A. Korinskiy - catap@catap.ru

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

(require 'jabber-chat)
(eval-when-compile (require 'cl))

(defvar jabber-log-lines-to-keep 1000
  "Maximum number of lines in chat buffer")

(defun jabber-clean-top ()
  "Clean old history from a chat buffer.
`jabber-log-lines-to-keep' specifies the number of lines to
keep."
  (interactive)
  (let ((inhibit-read-only t)
	(delete-before 
	 ;; go back one node, to make this function "idempotent"
	 (ewoc-prev
	  jabber-chat-ewoc
	  (ewoc-locate jabber-chat-ewoc
		       (save-excursion
			 (goto-char (point-max))
			 (forward-line (- jabber-log-lines-to-keep))
			 (point))))))
    (while delete-before 
      (setq delete-before
	    (prog1 
		(ewoc-prev jabber-chat-ewoc delete-before)
	      (ewoc-delete jabber-chat-ewoc delete-before))))))

(defun jabber-clean-muc (nick group buffer text proposed-alert)
  "Clean old history from MUC buffers.
`jabber-log-lines-to-keep' specifies the number of lines to
keep."
  (jabber-clean-top))

(pushnew 'jabber-clean-muc (get 'jabber-alert-muc-hooks 'custom-options))

(provide 'jabber-clean)

;; arch-tag: 3d1e3428-f598-11db-a314-000a95c2fcd0
