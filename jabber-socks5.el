;; jabber-socks5.el - SOCKS5 bytestreams by JEP-0065

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

(require 'jabber-iq)
(require 'jabber-disco)
(require 'jabber-si-server)

(defvar jabber-socks5-pending-sessions nil
  "List of pending sessions.

Each entry is a list, containing:
 * Stream ID
 * Full JID of initiator
 * Profile start function, to be called when session is activated")

(defvar jabber-socks5-active-sessions nil
  "List of active sessions.

This is an alist where the keys are (sid jid) and the values are
network streams.")

(add-to-list 'jabber-advertised-features "http://jabber.org/protocol/bytestreams")

(add-to-list 'jabber-si-stream-methods
	     (list "http://jabber.org/protocol/bytestreams"
		   'jabber-socks5-accept
		   'jabber-socks5-read))

(defun jabber-socks5-accept (jid sid profile-start-function)
  "Remember that we are waiting for connection from JID, with stream id SID"
  ;; asking the user for permission is done in the profile
  (add-to-list 'jabber-socks5-pending-sessions
	       (list sid jid profile-start-function)))

(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons "http://jabber.org/protocol/bytestreams" 'jabber-socks5-process))
(defun jabber-socks5-process (xml-data)
  "Accept IQ get for SOCKS5 bytestream"
  (let* ((jid (jabber-xml-get-attribute xml-data 'from))
	 (id (jabber-xml-get-attribute xml-data 'id))
	 (query (jabber-iq-query xml-data))
	 (sid (jabber-xml-get-attribute query 'sid))
	 (session (dolist (pending-session jabber-socks5-pending-sessions)
		    (when (and (equal sid (nth 0 pending-session))
			       (equal jid (nth 1 pending-session)))
		      (return pending-session)))))
    ;; check that we really are expecting this session
    (unless session
      (jabber-signal-error "auth" 'not-acceptable))

    (setq jabber-socks5-pending-sessions (delq session jabber-socks5-pending-sessions))
    ;; find streamhost to connect to
    (let* ((streamhosts (jabber-xml-get-children query 'streamhost))
	   (streamhost (dolist (streamhost streamhosts)
			 (if (jabber-socks5-connect streamhost jid sid)
			     (return streamhost)))))
      (unless streamhost
	(jabber-signal-error "cancel" 'item-not-found))
      
      ;; tell initiator which streamhost we use
      (jabber-send-iq jid "result"
		      `(query ((xmlns . "http://jabber.org/protocol/bytestreams"))
			      (streamhost-used ((jid . ,(jabber-xml-get-attribute streamhost 'jid)))))
		      nil nil nil nil id)
      ;; tell profile to start reading data
      (run-with-idle-timer 3 nil 
			   (nth 2 session) jid sid #'jabber-socks5-read))))

(defun jabber-socks5-connect (streamhost jid sid)
  "Attempt to connect to STREAMHOST, authenticating with JID and SID.
Return nil on error.  On success, store details in
`jabber-socks5-active-sessions'.

STREAMHOST has the form
\(streamhost ((host . HOST)
	     (port . PORT)))

Zeroconf is not supported."
  (message "Attempting SOCKS5 connection to %s (%s %s)" streamhost jid sid)
  (condition-case e
      (let ((coding-system-for-read 'binary)
	    (coding-system-for-write 'binary)
	    (host (jabber-xml-get-attribute streamhost 'host))
	    (port (string-to-number (jabber-xml-get-attribute streamhost 'port))))
	;; is this the best way to send binary network output?
	(let ((socks5-connection (open-network-stream "socks5" (generate-new-buffer-name "socks5") host port)))
	  (with-current-buffer (process-buffer socks5-connection)
	    ;; version: 5.  number of auth methods supported: 1.
	    ;; which one: no authentication.
	    (process-send-string socks5-connection (string 5 1 0))
	    ;; wait for response
	    (accept-process-output socks5-connection 15)
	    ;; should return:
	    ;; version: 5.  auth method to use: none
	    (unless (string= (buffer-substring 1 3) (string 5 0))
	      (error "SOCKS5 authentication required"))

	    ;; send connect command
	    (let ((hash (sha1-string (concat sid jid jabber-username "@" jabber-server "/" jabber-resource))))
	      (process-send-string 
	       socks5-connection
	       (concat (string 5 1 0 3 (length hash))
		       hash
		       (string 0 0))))

	    (accept-process-output socks5-connection 15)
	    (unless (string= (buffer-substring 3 5) (string 5 0))
	      (error "SOCKS5 failure"))

	    ;; The information returned here is exactly the same that we sent...
	    ;; Not very exciting.  Anyway, this part is done, we have a connection.
	    (let* ((address-type (aref (buffer-substring 6 7) 0))
		   (address-length (aref (buffer-substring 7 8) 0))
		   (address (buffer-substring 8 (+ 8 address-length)))
		   (address-port-string (buffer-substring (+ 8 address-length) (+ 8 address-length 2)))
		   (address-port (+
				  (* 256 (aref address-port-string 0))
				  (*   1 (aref address-port-string 1)))))
	      ;;(message "Address type: %d\nAddress: %s\nPort: %d" address-type address address-port)

	      ;; Delete all SOCKS5 data, leave room for the stream.
	      (delete-region 1 (+ 8 address-length 2)))

	    (set-process-sentinel socks5-connection #'jabber-socks5-sentinel)
	    (push (cons (list jid sid) socks5-connection)
		  jabber-socks5-active-sessions))))
    (error
     (message "SOCKS5 connection failed: %s" e))))

(defun jabber-socks5-read (jid sid)
  "Read chunk of data from the stream identified by JID and SID.
Return nil on EOF."
  (let ((session (assoc (list jid sid) jabber-socks5-active-sessions)))
    (if session
	(let ((stream (cdr session)))
	  (cond
	   ((stringp stream)
	    ;; The stream has been closed; return the remaining data.
	    (setcdr session nil)
	    stream)

	   ((null stream)
	    ;; The stream was closed the last time.  Remove it from
	    ;; the session table and return nil.
	    (setq jabber-socks5-active-sessions
		  (delq session jabber-socks5-active-sessions))
	    nil)

	   ((processp stream)
	    ;;(accept-process-output stream 5)
	    (let ((data (buffer-string (process-buffer stream))))
	      (delete-region 1 (1+ (length data)) (process-buffer stream))
	      data))))

      (if (assoc (list jid sid) jabber-socks5-pending-sessions)
	  ""
	(error "SOCKS5 session doesn't exist")))))

(defun jabber-socks5-sentinel (process event-string)
  ;; Connection terminated.  Shuffle together the remaining data,
  ;; and kill the buffer.
  (let ((session (rassq process jabber-socks5-active-sessions))
	(buffer (process-buffer process)))
    (with-current-buffer buffer
      (setcdr session (buffer-string)))
    (kill-buffer buffer)
    (delete-process process)))

(provide 'jabber-socks5)

;;; arch-tag: 9e70dfea-2522-40c6-a79f-302c8fb82ac5
