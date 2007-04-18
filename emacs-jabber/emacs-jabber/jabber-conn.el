;; jabber-conn.el - Network transport functions

;; Copyright (C) 2005 - Georg Lehner - jorge@magma.com.ni
;; mostly inspired by Gnus.

;; Copyright (C) 2005 - Carl Henrik Lunde - chlunde+jabber+@ping.uio.no
;; (starttls)

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

;; A collection of functions, that hide the details of transmitting to
;; and fro a Jabber Server

(eval-when-compile (require 'cl))

;; Try two different TLS/SSL libraries, but don't fail if none available.
(or (ignore-errors (require 'tls))
    (ignore-errors (require 'ssl)))

(ignore-errors (require 'starttls))

(require 'srv)

;; TODO: Add custom flag, to not complain about plain-text passwords
;;       in encrypted connections
;;

;; This variable holds the connection, which is used for further
;; input/output to the server
(defvar *jabber-connection* nil
  "the process that does the actual connection")

(defgroup jabber-conn nil "Jabber Connection Settings"
  :group 'jabber)

(defcustom jabber-network-server nil
  "hostname or IP address of server to connect to, if different from `jabber-server'."
  :type '(radio (const :tag "Same as `jabber-server'" nil)
		(string :tag "Hostname or IP address"))
  :group 'jabber-conn)

(defcustom jabber-port nil
  "jabber port
The default depends on the connection type: 5222 for ordinary connections
and 5223 for SSL connections."
  :type '(choice (const :tag "Default" nil)
		 (integer :tag "Port number"))
  :group 'jabber-conn)

(defcustom jabber-connection-type 
  (cond
   ;; Use STARTTLS if we can...
   ((and (featurep 'starttls)
	 (or (and (bound-and-true-p starttls-gnutls-program)
		  (executable-find starttls-gnutls-program))
	     (and (bound-and-true-p starttls-program)
		  (executable-find starttls-program))))
    'starttls)
   ;; ...else default to unencrypted connection.
   (t
    'network))
  "Type of connection to the jabber server, ssl or network most likely."
  :type '(radio (const :tag "Encrypted connection, SSL" ssl)
		(const :tag "Negotiate encrypted connection when available (STARTTLS)" starttls)
		(const :tag "Standard TCP/IP connection" network))
  :group 'jabber-conn)

(defcustom jabber-connection-ssl-program nil
  "Program used for SSL/TLS connections.
nil means prefer gnutls but fall back to openssl.
'gnutls' means use gnutls (through `open-tls-stream').
'openssl means use openssl (through `open-ssl-stream')."
  :type '(choice (const :tag "Prefer gnutls, fall back to openssl" nil)
		 (const :tag "Use gnutls" gnutls)
		 (const :tag "Use openssl" openssl))
  :group 'jabber-conn)

(defvar jabber-connect-methods
  '((network jabber-network-connect jabber-network-send)
    (starttls jabber-starttls-connect jabber-ssl-send)
    (ssl jabber-ssl-connect jabber-ssl-send))
  "Alist of connection methods and functions.
First item is the symbol naming the method.
Second item is the connect function.
Third item is the send function.")

(defun jabber-get-connect-function (type)
  "Get the connect function associated with TYPE.
TYPE is a symbol; see `jabber-connection-type'."
  (let ((entry (assq jabber-connection-type jabber-connect-methods)))
    (nth 1 entry)))

(defun jabber-get-send-function (type)
  "Get the send function associated with TYPE.
TYPE is a symbol; see `jabber-connection-type'."
  (let ((entry (assq jabber-connection-type jabber-connect-methods)))
    (nth 2 entry)))

(defun jabber-srv-targets (server)
  "Find host and port to connect to.
If we can't find SRV records, use standard defaults."
  ;; XXX: per account
  ;; If the user has specified a host or a port, obey that.
  (if (or jabber-network-server jabber-port)
      (list (cons (or jabber-network-server server)
		  (or jabber-port 5222)))
    (or (condition-case nil
	    (srv-lookup (concat "_xmpp-client._tcp." server))
	  (error nil))
	(list (cons server 5222)))))

;; Plain TCP/IP connection
(defun jabber-network-connect (fsm server)
  "Connect to a Jabber server with a plain network connection.
Send a message of the form (:connected CONNECTION) to FSM if
connection succeeds.  Send a message :connection-failed if
connection fails."
  ;; XXX: asynchronous connection
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(targets (jabber-srv-targets server)))
    (catch 'connected
      (dolist (target targets)
	(condition-case e
	    (let ((connection 
		   (open-network-stream
		    "jabber"
		    (generate-new-buffer jabber-process-buffer)
		    (car target)
		    (cdr target))))
	      (when connection
		(fsm-send fsm (list :connected connection))
		(throw 'connected connection)))
	  (error
	   (message "Couldn't connect to %s: %s" target
		    (error-message-string e)))))
      (fsm-send fsm :connection-failed))))

(defun jabber-network-send (connection string)
  "Send a string via a plain TCP/IP connection to the Jabber Server."
  (process-send-string connection string))

;; SSL connection, we use openssl's s_client function for encryption
;; of the link
;; TODO: make this configurable
(defun jabber-ssl-connect (fsm server)
  "connect via OpenSSL or GnuTLS to a Jabber Server
Send a message of the form (:connected CONNECTION) to FSM if
connection succeeds.  Send a message :connection-failed if
connection fails."
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(connect-function
	 (cond
	  ((and (memq jabber-connection-ssl-program '(nil gnutls))
		(fboundp 'open-tls-stream))
	   'open-tls-stream)
	  ((and (memq jabber-connection-ssl-program '(nil openssl))
		(fboundp 'open-ssl-stream))
	   'open-ssl-stream)
	  (t
	   (error "Neither TLS nor SSL connect functions available")))))
    (setq *jabber-encrypted* t)
    (let ((connection
	   (funcall connect-function
		    "jabber"
		    (generate-new-buffer jabber-process-buffer)
		    (or jabber-network-server server)
		    (or jabber-port 5223))))
      (if connection
	  (fsm-send fsm (list :connected connection))
	(fsm-send fsm :connection-failed)))))

(defun jabber-ssl-send (connection string)
  "Send a string via an SSL-encrypted connection to the Jabber Server."
  ;; It seems we need to send a linefeed afterwards.
  (process-send-string connection string)
  (process-send-string connection "\n"))

(defun jabber-starttls-connect (fsm server)
  "Connect via GnuTLS to a Jabber Server.
Send a message of the form (:connected CONNECTION) to FSM if
connection succeeds.  Send a message :connection-failed if
connection fails."
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(targets (jabber-srv-targets server)))
    (unless (fboundp 'starttls-open-stream)
      (error "starttls.el not available"))
    (catch 'connected
      (dolist (target targets)
	(condition-case e
	    (let ((connection
		   (starttls-open-stream
		    "jabber"
		    (generate-new-buffer jabber-process-buffer)
		    (car target)
		    (cdr target))))
	      (when connection
		(fsm-send fsm (list :connected connection))
		(throw 'connected connection)))
	  (error
	   (message "Couldn't connect to %s: %s" target
		    (error-message-string e))))
	(fsm-send fsm :connection-failed)))))

(defun jabber-starttls-initiate (fsm)
  "Initiate a starttls connection"
  (jabber-send-sexp fsm
   '(starttls ((xmlns . "urn:ietf:params:xml:ns:xmpp-tls")))))

(defun jabber-starttls-process-input (fsm xml-data)
  "Process result of starttls request.
Return non-nil on success, nil on failure."
  (cond
   ((eq (car xml-data) 'proceed)
    (starttls-negotiate (plist-get (fsm-get-state-data fsm) :connection)))
   ((eq (car xml-data) 'failure)
    nil)))

(provide 'jabber-conn)
;; arch-tag: f95ec240-8cd3-11d9-9dbf-000a95c2fcd0
