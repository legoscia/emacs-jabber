(require 'jabberd)
(require 'cl)

(setq jabber-roster-show-bindings nil)

(jabberd-connect)

(with-timeout (5 (progn
		   (princ (with-current-buffer "*fsm-debug*" (buffer-string)))
		   (error "Timeout")))
  (while (not (equal "" *jabber-current-show*))
    (sit-for 0.1)))

(princ (format "in %s now\n" default-directory))

(defun rd-clear-roster ()
  (let ((state-data (fsm-get-state-data (car jabber-connections))))
    ;; First unintern everything:
    (jabber-clear-roster)
    (plist-put state-data :roster nil)
    (plist-put state-data :roster-hash nil)))

(let* ((program (expand-file-name "roster-display" (file-name-directory load-file-name)))
       (p (if (not (file-executable-p program))
	      (progn
		(princ
		 (format "%s not found or not executable; skipping Quickcheck test\n"
			 program))
		;; Exit code 77 means "skip" to automake
		(kill-emacs 77))
	    (start-process "roster-display" "*roster-display*" program)))
       done)
  (with-current-buffer (process-buffer p)
    (while (not done)
      (while (progn (goto-char (point-min)) (not (search-forward-regexp "^[a-z]" nil t)))
	(accept-process-output p))
      (goto-char (match-beginning 0))
      (cond
       ((looking-at "success")
	(setq done t)
	(princ "Success!\n"))
       ((looking-at "failure")
	(while (process-live-p p)
	  (accept-process-output p))
	(princ (buffer-substring (point) (point-max)))
	(error "it failed"))
       ((looking-at "check")
	(let ((all-messages-s (delete-and-extract-region (point-min) (point)))
	      all-messages
	      roster-1 roster-2)
	  (delete-region (point-min) (point-max))
	  (with-temp-buffer
	    (insert all-messages-s)
	    (goto-char (point-min))
	    (while
		(condition-case e
		    (push (read (current-buffer)) all-messages)
		  (end-of-file
		   nil))))
	  (setq all-messages (nreverse all-messages))
	  (dolist (m all-messages)
	    (jabber-process-input (car jabber-connections) m))

	  ;; The presence stanza causes an asynchronous :roster-update message
	  ;; to be sent.  Let's wait for that.
	  (accept-process-output nil 0.1)

	  ;; Roster updates are batched.  Force a timeout.
	  (fsm-send-sync (car jabber-connections) :timeout)

	  (with-current-buffer jabber-roster-buffer
	    (setq roster-1 (buffer-substring-no-properties (point-min) (point-max))))

	  (jabber-display-roster)

	  (with-current-buffer jabber-roster-buffer
	    (setq roster-2 (buffer-substring-no-properties (point-min) (point-max))))

	  (if (equal roster-1 roster-2)
	      (process-send-string p "t\n")
	    (let ((result (mismatch roster-1 roster-2)))
	      (if (null result)
		  (princ "match\n")
		(princ "mismatch!  Expected:\n")
		(prin1 roster-2)
		(princ "\nBut got:\n")
		(prin1 (substring roster-1 0 result))
		(princ " ***mismatch here*** ")
		(prin1 (substring roster-1 result))
		(princ "\n")))
	    (process-send-string p "nil\n"))
	  (rd-clear-roster)

	  (jabber-disconnect)
	  (jabberd-connect)

	  (setq *jabber-current-show* nil)
	  (with-timeout (5 (progn
			     (princ (with-current-buffer "*fsm-debug*" (buffer-string)))
			     (error "Timeout")))
	    (while (not (equal "" *jabber-current-show*))
	      (sit-for 0.1)))

	  (jabber-display-roster)))
       (t
	(princ (concat "What's that?\n'" (buffer-substring (point) (point-max))))
	(error "???"))))))
