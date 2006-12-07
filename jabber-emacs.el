;; -*- mode: emacs-lisp -*-

(require 'jabber)
(require 'jabber-export)

(add-hook 'jabber-chat-mode-hook
          '(lambda ()
             (local-set-key [(tab)] 'dabbrev-completion)))

(defun jabber-roster-reload ()
  "Reload roster in current buffer"
  (interactive)
  (jabber-roster-mode)
  (jabber-display-roster))
