;; jabber-ft-server.el - handle incoming file transfers, by JEP-0096
;; $Id: jabber-ft-server.el,v 1.1 2004/04/08 12:01:24 legoscia Exp $

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

(require 'jabber-si-server)
(require 'jabber-util)

(add-to-list 'jabber-advertised-features "http://jabber.org/protocol/si/profile/file-transfer")

(add-to-list 'jabber-si-profiles
	     (list "http://jabber.org/protocol/si/profile/file-transfer"
		   'jabber-ft-accept))
(defun jabber-ft-accept (xml-data)
  "Receive IQ stanza containing file transfer request, ask user"
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
	 (query (jabber-iq-query xml-data))
	 ;; TODO: check namespace
	 (file (car (jabber-xml-get-children query 'file)))
	 (name (jabber-xml-get-attribute file 'name))
	 (size (jabber-xml-get-attribute file 'size))
	 (date (jabber-xml-get-attribute file 'date))
	 (md5-hash (jabber-xml-get-attribute file 'hash))
	 (desc (car (jabber-xml-node-children
		     (car (jabber-xml-get-children file 'desc)))))
	 (range (car (jabber-xml-get-children file 'range))))
    (unless (and name size)
      ;; both name and size must be present
      (jabber-signal-error "modify" 'bad-request))

    (let ((question (format
		     "%s is sending you the file %s (%s bytes).%s  Accept?"
		     (jabber-jid-displayname from)
		     name
		     size
		     (if (not (zerop (length desc)))
			 (concat "  Description: '" desc "'")
		       ""))))
      (unless (yes-or-no-p question)
	(jabber-signal-error "cancel" 'forbidden)))

    ;; TODO: ask for filename, open buffer, record SID+JID

    ;; to support range, return something sensible here
    nil))

(provide 'jabber-ft-server)
