;; jabber-chatbuffer.el - functions common to all chat buffers

;; Copyright (C) 2005 - Magnus Henoch - mange@freemail.hu

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

(defvar jabber-point-insert nil
  "Position where the message being composed starts")

(defvar jabber-send-function nil
  "Function for sending a message from a chat buffer.")

(defvar jabber-chat-mode-hook nil
  "Hook called at the end of `jabber-chat-mode'.
Note that functions in this hook have no way of knowing
what kind of chat buffer is being created.")

(defun jabber-chat-mode ()
  "\\{jabber-chat-mode-map}"
  (kill-all-local-variables)
  ;; Make sure to set this variable somewhere
  (make-local-variable 'jabber-send-function)

  (make-local-variable 'scroll-conservatively)
  (setq scroll-conservatively 5)

  (make-local-variable 'jabber-point-insert)
  (setq jabber-point-insert (point-min))

  ;;(setq header-line-format jabber-chat-header-line-format)

  (setq major-mode 'jabber-chat-mode
        mode-name "jabber-chat")
  (use-local-map jabber-chat-mode-map)

  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'jabber-chat-mode-hook)
    (run-hooks 'jabber-chat-mode-hook)))

(put 'jabber-chat-mode 'mode-class 'special)

;; Spell check only what you're currently writing
(defun jabber-chat-mode-flyspell-verify ()
  (>= (point) jabber-point-insert))
(put 'jabber-chat-mode 'flyspell-mode-predicate
  'jabber-chat-mode-flyspell-verify)

(defvar jabber-chat-mode-map (copy-keymap jabber-common-keymap))

(define-key jabber-chat-mode-map "\r" 'jabber-chat-buffer-send)

(defun jabber-chat-buffer-send ()
  (interactive)
  (let ((body (delete-and-extract-region jabber-point-insert (point-max))))
    ;; If user accidentally hits RET without writing anything,
    ;; delete-and-extract-region returns "".  In that case,
    ;; no message should be sent.
    (unless (zerop (length body))
      (funcall jabber-send-function body))))

(defun jabber-chat-buffer-display (prompt-function prompt-data output-functions output-data)
  "Display a message in current buffer.
PROMPT-FUNCTION is a function that prints the correct prompt at
point.  It is called with PROMPT-DATA as argument.
OUTPUT-FUNCTIONS is a list of functions that may or may not print something
at point.  They are called in order with OUTPUT-DATA as argument.
If the OUTPUT-FUNCTIONS produce any output, PROMPT-FUNCTION is called
with point before that output.  If there is no output, there is
no prompt.

If point is at or after jabber-point-insert, it is advanced.
If point is before jabber-point-insert, it is not moved."
  (let ((at-insert-point (eq (point) jabber-point-insert)))
    (save-excursion
      (goto-char jabber-point-insert)
      (jabber-chat-buffer-display-at-point prompt-function prompt-data output-functions output-data)
      (setq jabber-point-insert (point))
      (set-text-properties jabber-point-insert (point-max) nil))

    (when at-insert-point
      (goto-char jabber-point-insert))))

(defun jabber-chat-buffer-display-at-point (prompt-function prompt-data output-functions output-data)
  "Display a message at point.
Arguments are as to `jabber-chat-buffer-display'."
  (let ((inhibit-read-only t)
	(beg (point))
	(point-insert (set-marker (make-marker) jabber-point-insert)))
    (set-marker-insertion-type point-insert t)

    (dolist (printer output-functions)
      (funcall printer output-data)
      (unless (bolp)
	(insert "\n")))

    (unless (eq (point) beg)
      (let ((end (point-marker)))
	(goto-char beg)
	(funcall prompt-function prompt-data)
	(goto-char end)
	(put-text-property beg end 'read-only t)
	(put-text-property beg end 'front-sticky t)
	(put-text-property beg end 'rear-nonsticky t)
	(setq jabber-point-insert (marker-position point-insert))))))

(provide 'jabber-chatbuffer)
;; arch-tag: 917e5b60-5894-4c49-b3bc-12e1f97ffdc6
