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
 * Profile data function, to be called when data is received")

(defvar jabber-socks5-active-sessions nil
  "List of active sessions.

Each entry is a list, containing:
 * Network connection
 * Stream ID
 * Full JID of initiator
 * Profile data function")

(add-to-list 'jabber-advertised-features "http://jabber.org/protocol/bytestreams")

(add-to-list 'jabber-si-stream-methods
	     (list "http://jabber.org/protocol/bytestreams"
		   'jabber-socks5-accept))

(defun jabber-socks5-accept (jid sid profile-data-function)
  "Remember that we are waiting for connection from JID, with stream id SID"
  ;; asking the user for permission is done in the profile
  (add-to-list 'jabber-socks5-pending-sessions
	       (list sid jid profile-data-function)))

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
		      (return pending-session))))
	 (profile-data-function (nth 2 session)))
    ;; check that we really are expecting this session
    (unless session
      (jabber-signal-error "auth" 'not-acceptable))

    (setq jabber-socks5-pending-sessions (delq session jabber-socks5-pending-sessions))
    ;; find streamhost to connect to
    (let* ((streamhosts (jabber-xml-get-children query 'streamhost))
	   (streamhost (dolist (streamhost streamhosts)
			 (if (jabber-socks5-connect streamhost sid jid profile-data-function)
			     (return streamhost)))))
      (unless streamhost
	(jabber-signal-error "cancel" 'item-not-found))
      
      ;; tell initiator which streamhost we use
      (jabber-send-iq jid "result"
		      `(query ((xmlns . "http://jabber.org/protocol/bytestreams"))
			      (streamhost-used ((jid . ,(jabber-xml-get-attribute streamhost 'jid)))))
		      nil nil nil nil id)
      ;; now, as data is sent, it will be passed to the profile.
      )))

(defun jabber-socks5-connect (streamhost sid jid profile-data-function)
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

	    (message "SOCKS5 connection established")

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

	    ;; We immediately claim that, now that this connection is
	    ;; successfully established, it is the one to use for this
	    ;; transfer.  This is usually what we want, but
	    ;; theoretically those two facts are orthogonal.
	    (push (list socks5-connection sid jid profile-data-function)
		  jabber-socks5-active-sessions)

	    ;; If more data than the SOCKS5 response has arrived, pass it to the filter.
	    ;; This shouldn't happen, as we are supposed to send a confirmation first,
	    ;; but you never know...
	    (when (not (zerop (buffer-size)))
	      (jabber-socks5-filter socks5-connection (buffer-string))
	      (erase-buffer))

	    ;; Now set the filter, for the rest of the output
	    (set-process-filter socks5-connection #'jabber-socks5-filter)
	    (set-process-sentinel socks5-connection #'jabber-socks5-sentinel))))
    (error
     (message "SOCKS5 connection failed: %s" e))))

(defun jabber-socks5-filter (connection data)
  "Pass data from connection to profile data function"
  (let* ((session (assq connection jabber-socks5-active-sessions))
	 (sid (nth 1 session))
	 (jid (nth 2 session))
	 (profile-data-function (nth 3 session)))
    (funcall profile-data-function jid sid data)))

(defun jabber-socks5-sentinel (process event-string)
  ;; Connection terminated.  Shuffle together the remaining data,
  ;; and kill the buffer.
  (let* ((session (assq process jabber-socks5-active-sessions))
	 (buffer (process-buffer process))
	 (sid (nth 1 session))
	 (jid (nth 2 session))
	 (profile-data-function (nth 3 session)))
    (kill-buffer buffer)
    (delete-process process)
    (funcall profile-data-function jid sid nil)
    (setq jabber-socks5-active-sessions (delq session jabber-socks5-pending-sessions))))

(provide 'jabber-socks5)

;;; arch-tag: 9e70dfea-2522-40c6-a79f-302c8fb82ac5
