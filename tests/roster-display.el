(require 'jabberd)

(setq jabber-roster-show-bindings nil)

;; jabber-post-connect-hooks is run after the roster has been drawn.
(add-hook 'jabber-post-connect-hooks 'rd-check-roster-buffer)

(defvar rd-roster-string nil)

(defconst rd-expected-roster
  (concat
   "Jabber roster\n"
   "__________________________________\n"
   "\n"
   " - Offline -\n"
   "romeo@montague.net\n"
   "__________________________________\n"
   "\n"
   "__________________________________\n"
   "\n"))

(defun rd-check-roster-buffer (_jc)
  (with-current-buffer jabber-roster-buffer
    (let ((contents (buffer-string)))
      (set-text-properties 0 (length contents) () contents)
      (prin1 contents)
      (setq rd-roster-string contents))))

(jabberd-connect)

(with-timeout (5 (error "Timeout"))
  (while (not rd-roster-string)
    (sit-for 0.1)))

(unless (equal rd-roster-string rd-expected-roster)
  (error "Bad roster"))
