;; jabber-socks5.el - SOCKS5 bytestreams by JEP-0065
;; $Id: jabber-socks5.el,v 1.1 2004/04/07 20:03:51 legoscia Exp $

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
 * Full JID of initiator")

(add-to-list 'jabber-advertised-features "http://jabber.org/protocol/bytestreams")

(add-to-list 'jabber-si-stream-methods
	     (list "http://jabber.org/protocol/bytestreams" 'jabber-socks5-accept))

(defun jabber-socks5-accept (jid sid)
  "Remember that we are waiting for connection from JID, with stream id SID"
  ;; asking the user for permission is done in the profile
  (add-to-list 'jabber-socks5-pending-sessions
	       (list sid jid)))

(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons "http://jabber.org/protocol/bytestreams" 'jabber-socks5-process))
(defun jabber-socks5-process (xml-data)
  "Accept IQ get for SOCKS5 bytestream"
  (let* ((jid (jabber-xml-get-attribute xml-data 'from))
	 (id (jabber-xml-get-attribute xml-data 'id))
	 (query (jabber-iq-query xml-data))
	 (sid (jabber-xml-get-attribute query 'sid)))
    (unless (member (list sid jid) jabber-socks5-pending-sessions)
      (jabber-signal-error 'auth 'not-acceptable))

    (setq jabber-socks5-pending-sessions (delete (list sid jid) jabber-socks5-pending-sessions))
    (let ((streamhosts (jabber-xml-get-children query 'streamhost)))
      (dolist (streamhost streamhosts)
	(if (jabber-socks5-connect streamhost jid sid)
	    (return streamhost)))
      ;; more to do here
      )))

(provide 'jabber-socks5)
