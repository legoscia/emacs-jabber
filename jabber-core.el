;; jabber-core.el - core functions
;; $Id: jabber-core.el,v 1.4 2004/03/27 22:55:37 legoscia Exp $

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

(defvar *jabber-connection* nil
  "the process that does the actual connection")

(defvar *jabber-roster* nil
  "the roster list")

(defvar jabber-jid-obarray (make-vector 127 0)
  "obarray for keeping JIDs")

(defvar *jabber-connected* nil
  "boolean - are we connected")

(defvar *xmlq* ""
  "a string with all the incoming xml that is waiting to be parsed")

(defvar jabber-register-p nil
  "Register a new account in this session?")

(defvar jabber-session-id nil
  "id of the current session")

(defvar jabber-register-p nil
  "Is account registration occurring in this session?")

(defvar jabber-call-on-connection nil
  "Function to be called on connection.
This is set by `jabber-connect' on each call, and later picked up in
`jabber-filter'.")

;; (defgroup jabber-core nil "customize core functionality"
;;   :group 'jabber)

(defvar jabber-message-chain nil
  "Incoming messages are sent to these functions, in order.")

(defvar jabber-iq-chain nil
  "Incoming infoqueries are sent to these functions, in order.")

(defvar jabber-presence-chain (list 'jabber-process-presence)
  "Incoming presence notifications are sent to these functions, in order.")

(defun jabber-connect (&optional registerp)
  "connect to the jabber server and start a jabber xml stream
With prefix argument, register a new account."
  (interactive "P")
  (if *jabber-connected*
      (message "Already connected")
    (setq *xmlq* "")
    (jabber-clear-roster)
    (let ((coding-system-for-read 'utf-8)
	  (coding-system-for-write 'utf-8))
      (setq *jabber-connection* (open-network-stream "jabber"
						     "*-jabber-*"
						     (or jabber-network-server jabber-server)
						     jabber-port)))
    (set-process-filter *jabber-connection* #'jabber-filter)
    (set-process-sentinel *jabber-connection* #'jabber-sentinel)

    (setq jabber-register-p registerp)
    (setq jabber-call-on-connection (if registerp
					#'(lambda () (jabber-get-register jabber-server))
				      #'(lambda () (jabber-get-auth jabber-server))))
    (process-send-string *jabber-connection*
			 (concat "<?xml version='1.0'?><stream:stream to='" 
				 jabber-server 
				 "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>"))
    ;; Next thing happening is the server sending its own <stream:stream> start tag.
    ;; That is handled in jabber-filter.

    (setq *jabber-connected* t)))

(defun jabber-disconnect ()
  "disconnect from the jabber server and re-initialise the jabber package variables"
  (interactive)
  (when (eq (process-status *jabber-connection*) 'open)
    (process-send-string *jabber-connection* "</stream:stream>")
    (delete-process *jabber-connection*))
  (kill-buffer (process-buffer *jabber-connection*))
  (jabber-clear-roster)
  (setq *xmlq* "")
  (setq *jabber-connected* nil)
  (setq *jabber-active-groupchats* nil)
  (setq jabber-session-id nil)
  (if (interactive-p)
      (message "Disconnected from Jabber server")))

(defun jabber-sentinel (process event)
  "alert user about lost connection"
  (beep)
  (message "Jabber connection lost: `%s'" event)
  (jabber-disconnect))

(defun jabber-filter (process string)
  "the filter function for the jabber process"
  (cond
   ((string-match "</stream:stream>" string)
    (jabber-disconnect))
   ((string-match "<stream:stream" string)
    (setq jabber-session-id
          (progn (string-match "id='\\([A-Za-z0-9]+\\)'" string)
               (match-string 1 string)))
    ;; Now proceed with logon.
    (funcall jabber-call-on-connection))
   (t
    (if (active-minibuffer-window)
        (run-with-idle-timer 0.01 nil #'jabber-filter process string)
      (with-temp-buffer
        (setq *xmlq* (concat *xmlq* string))
        (if (string-match " \\w+=''" *xmlq*)
            (setq *xmlq* (replace-match "" nil nil *xmlq*)))
        (catch 'jabber-no-tag
          (while (string-match "<\\([a-zA-Z0-9\:]+\\)\\s-" *xmlq*)
            (if (or (string-match (concat "<" (match-string 1 *xmlq*) "[^<>]*?/>") *xmlq*)
                    (string-match (concat "<" (match-string 1 *xmlq*) ".*?>[^\0]+?</" (match-string 1 *xmlq*) ">") *xmlq*))
                (progn
                  (insert (match-string 0 *xmlq*))
                  (goto-char (point-min))
                  (setq *xmlq* (substring *xmlq* (match-end 0)))
                  (let ((xml-data (xml-parse-region (point-min)
                                                    (point-max))))
                    (if xml-data
                        (progn
                          (if jabber-debug-log-xml
			      (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
				(save-excursion
				  (goto-char (point-max))
				  (insert (format "receive %S\n\n" (car xml-data))))))
                          (jabber-process-input (car xml-data)))))
                  (erase-buffer))
              (throw 'jabber-no-tag t)))))))))

(defun jabber-process-input (xml-data)
  "process an incoming parsed tag"
  (let* ((tag (jabber-xml-node-name xml-data))
	 (functions (eval (cdr (assq tag '((iq . jabber-iq-chain)
					   (presence . jabber-presence-chain)
					   (message . jabber-message-chain)))))))
    (dolist (f functions)
      (funcall f xml-data))))

(defun jabber-clear-roster ()
  "Clean up the roster.
This is made complicated by the fact that the JIDs are symbols with properties."
  (mapatoms #'(lambda (x)
		(unintern x jabber-jid-obarray))
	    jabber-jid-obarray)
  (setq *jabber-roster* nil))

(defun jabber-send-sexp (sexp)
  "send the xml corresponding to SEXP to the jabber server"
  (if jabber-debug-log-xml
      (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
	(save-excursion
	  (goto-char (point-max))
	  (insert (format "sending %S\n\n" sexp)))))
  (process-send-string *jabber-connection* (jabber-sexp2xml sexp)))

(provide 'jabber-core)
