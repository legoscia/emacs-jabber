;;; jabber-newdisco.el --- caching disco API

;; Copyright (C) 2005, 2008  Magnus Henoch

;; Author: Magnus Henoch <mange@freemail.hu>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;---
;; Keys are ("jid" . "node"), where "node" is nil if appropriate.
;; Values are (identities features), where each identity is ["name"
;; "category" "type"], and each feature is a string.
(defvar jabber-disco-info-cache (make-hash-table :test 'equal))

;; Keys are ("jid" . "node").  Values are (items), where each
;; item is ["name" "jid" "node"] (some values may be nil).
(defvar jabber-disco-items-cache (make-hash-table :test 'equal))

;;; Info

(defun jabber-disco-get-info (jc jid node callback closure-data &optional force)
  "Get disco info for JID and NODE, using connection JC.
Call CALLBACK with JC and CLOSURE-DATA as first and second
arguments and result as third argument when result is available.
On success, result is (IDENTITIES FEATURES), where each identity is [\"name\"
\"category\" \"type\"], and each feature is a string.
On error, result is the error node, recognizable by (eq (car result) 'error).

If CALLBACK is nil, just fetch data.  If FORCE is non-nil,
invalidate cache and get fresh data."
  (when force
    (remhash (cons jid node) jabber-disco-info-cache))
  (let ((result (gethash (cons jid node) jabber-disco-info-cache)))
    (if result
	(and callback (run-with-timer 0 nil callback jc closure-data result))
      (jabber-send-iq jc jid
		      "get"
		      `(query ((xmlns . "http://jabber.org/protocol/disco#info")
			       ,(when node `(node . ,node))))
		      #'jabber-disco-got-info (cons callback closure-data)
		      (lambda (jc xml-data callback-data)
			(when (car callback-data)
			  (funcall (car callback-data) jc (cdr callback-data) (jabber-iq-error xml-data))))
		      (cons callback closure-data)))))

(defun jabber-disco-got-info (jc xml-data callback-data)
  (let ((jid (jabber-xml-get-attribute xml-data 'from))
	(node (jabber-xml-get-attribute (jabber-iq-query xml-data)
					'node))
	(result
	 (list
	  (mapcar 
	   #'(lambda (id)
	       (vector (jabber-xml-get-attribute id 'name)
		       (jabber-xml-get-attribute id 'category)
		       (jabber-xml-get-attribute id 'type)))
	   (jabber-xml-get-children (jabber-iq-query xml-data) 'identity))
	  (mapcar
	   #'(lambda (feature)
	       (jabber-xml-get-attribute feature 'var))
	   (jabber-xml-get-children (jabber-iq-query xml-data) 'feature)))))
    (puthash (cons jid node) result jabber-disco-info-cache)
    (when (car callback-data)
      (funcall (car callback-data) jc (cdr callback-data) result))))

(defun jabber-disco-get-info-immediately (jid node)
  "Get cached disco info for JID and NODE.
Return nil if no info available.

Fill the cache with `jabber-disco-get-info'."
  (gethash (cons jid node) jabber-disco-info-cache))

;;; Items

(defun jabber-disco-get-items (jc jid node callback closure-data &optional force)
  "Get disco items for JID and NODE, using connection JC.
Call CALLBACK with JC and CLOSURE-DATA as first and second
arguments and items result as third argument when result is
available.
On success, result is a list of items, where each
item is [\"name\" \"jid\" \"node\"] (some values may be nil).
On error, result is the error node, recognizable by (eq (car result) 'error).

If CALLBACK is nil, just fetch data.  If FORCE is non-nil,
invalidate cache and get fresh data."
  (when force
    (remhash (cons jid node) jabber-disco-items-cache))
  (let ((result (gethash (cons jid node) jabber-disco-items-cache)))
    (if result
	(and callback (run-with-timer 0 nil callback jc closure-data result))
      (jabber-send-iq jc jid
		      "get"
		      `(query ((xmlns . "http://jabber.org/protocol/disco#items")
			       ,(when node `(node . ,node))))
		      #'jabber-disco-got-items (cons callback closure-data)
		      (lambda (jc xml-data callback-data)
			(when (car callback-data)
			  (funcall (car callback-data) jc (cdr callback-data) (jabber-iq-error xml-data))))
		      (cons callback closure-data)))))

(defun jabber-disco-got-items (jc xml-data callback-data)
  (let ((jid (jabber-xml-get-attribute xml-data 'from))
	(node (jabber-xml-get-attribute (jabber-iq-query xml-data)
					'node))
	(result
	 (mapcar
	  #'(lambda (item)
	      (vector
	       (jabber-xml-get-attribute item 'name)
	       (jabber-xml-get-attribute item 'jid)
	       (jabber-xml-get-attribute item 'node)))
	  (jabber-xml-get-children (jabber-iq-query xml-data) 'item))))
    (puthash (cons jid node) result jabber-disco-items-cache)
    (when (car callback-data)
      (funcall (car callback-data) jc (cdr callback-data) result))))

(defun jabber-disco-get-items-immediately (jid node)
  (gethash (cons jid node) jabber-disco-items-cache))

;;; Publish

(defun jabber-disco-publish (jc node item-name item-jid item-node)
  "Publish the given item under disco node NODE."
  (jabber-send-iq jc nil
		  "set"
		  `(query ((xmlns . "http://jabber.org/protocol/disco#items")
			   ,(when node `(node . ,node)))
			  (item ((action . "update")
				 (jid . ,item-jid)
				 ,(when item-name
				    `(name . ,item-name))
				 ,(when item-node
				    `(node . ,item-node)))))
		  'jabber-report-success "Disco publish"
		  'jabber-report-success "Disco publish"))

(defun jabber-disco-publish-remove (jc node item-jid item-node)
  "Remove the given item from published disco items."
  (jabber-send-iq jc nil
		  "set"
		  `(query ((xmlns . "http://jabber.org/protocol/disco#items")
			   ,(when node `(node . ,node)))
			  (item ((action . "remove")
				 (jid . ,item-jid)
				 ,(when item-node
				    `(node . ,item-node)))))
		  'jabber-report-success "Disco removal"
		  'jabber-report-success "Disco removal"))

(provide 'jabber-newdisco)

;; arch-tag: b47c06aa-cae6-11d9-b1c0-000a95c2fcd0
