;; jabber-disco.el - service discovery functions
;; $Id: jabber-disco.el,v 1.1 2004/02/25 21:42:02 legoscia Exp $

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


;;; All this should be seriously rewritten, or at least reconsidered.  I'm
;;; imagining a separation between backend and frontend, so that various
;;; functions can perform disco queries for their own purposes, and maybe
;;; some caching with that.

(require 'jabber-iq)

;; Advertise your features here.  Add the namespace to this list.
(defvar jabber-advertised-features
  (list "http://jabber.org/protocol/disco#info")
  "Features advertised on service discovery requests")

(defun jabber-process-disco-info (xml-data)
  "Handle results from info disco requests."

  (let ((beginning (point)))
    (dolist (x (jabber-xml-node-children (jabber-iq-query xml-data)))
      (cond
       ((eq (jabber-xml-node-name x) 'identity)
	(let ((name (jabber-xml-get-attribute x 'name))
	      (category (jabber-xml-get-attribute x 'category))
	      (type (jabber-xml-get-attribute x 'type)))
	  (insert (jabber-propertize (if name
				  (jabber-unescape-xml name)
					; tsk, tsk... name is _required_
				"Unnamed (remote entity violates JEP-0030)")
			      'face 'jabber-title-medium)
		  "\n\nCategory:\t" category "\n")
	  (if type
	      (insert "Type:\t\t" type "\n"))
	  (insert "\n")))
       ((eq (jabber-xml-node-name x) 'feature)
	(let ((var (jabber-xml-get-attribute x 'var)))
	  (insert "Feature:\t" var "\n")))))
    (put-text-property beginning (point) 'jabber-jid (jabber-xml-get-attribute xml-data 'from))))

(defun jabber-process-disco-items (xml-data)
  "Handle results from items disco requests."

  (let ((items (jabber-xml-get-children (jabber-iq-query xml-data) 'item)))
    (if items
	(dolist (item items)
	  (let ((jid (jabber-xml-get-attribute item 'jid))
		(name (jabber-xml-get-attribute item 'name))
		(node (jabber-xml-get-attribute item 'node)))
	    (insert 
	     (jabber-propertize 
	      (concat
	       (jabber-propertize
		(concat jid "\n" (if node (format "Node: %s\n" node)))
		'face 'jabber-title-medium)
	       (jabber-unescape-xml name) "\n\n")
	      'jabber-jid jid
	      'jabber-node node))))
      (insert "No items found.\n"))))

(add-to-list 'jabber-iq-get-xmlns-alist
	     (cons "http://jabber.org/protocol/disco#info" 'jabber-return-disco-info))
(defun jabber-return-disco-info (xml-data)
  "Respond to a service discovery request.
See JEP-0030."
  ;; TODO: nodes
  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id)))
    (jabber-send-iq to "result"
		    `(query ((xmlns . "http://jabber.org/protocol/disco#info"))
			    ;; If running under a window system, this is
			    ;; a GUI client.  If not, it is a console client.
			    (identity ((category . "client")
				       (name . "Emacs Jabber client")
				       (type . ,(if (memq window-system
							  '(x w32 mac))
						    "pc"
						  "console"))))
			    ,(mapcar
			      (lambda (featurename)
				`(feature ((var . ,featurename))))
			      jabber-advertised-features))
		    nil nil nil nil id)))

(defun jabber-get-disco-items (to &optional node)
  "Send a service discovery request for items"
  (interactive (list (jabber-read-jid-completing "Send items disco request to: ")
		     (jabber-read-node "Node (or leave empty): ")))
  (jabber-send-iq to
		  "get"
		  (list 'query (append (list (cons 'xmlns "http://jabber.org/protocol/disco#items"))
				       (if (> (length node) 0)
					   (list (cons 'node node)))))
		  #'jabber-process-data #'jabber-process-disco-items
		  #'jabber-process-data "Item discovery failed"))

(defun jabber-get-disco-info (to &optional node)
  "Send a service discovery request for info"
  (interactive (list (jabber-read-jid-completing "Send info disco request to: ")
		     (jabber-read-node "Node (or leave empty): ")))
  (jabber-send-iq to
		  "get"
		  (list 'query (append (list (cons 'xmlns "http://jabber.org/protocol/disco#info"))
				       (if (> (length node) 0)
					   (list (cons 'node node)))))
		  #'jabber-process-data #'jabber-process-disco-info
		  #'jabber-process-data "Info discovery failed"))

(provide 'jabber-disco)
