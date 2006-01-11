;;; jabber-private.el --- jabber:iq:private API by JEP-0049

;; Copyright (C) 2005  Magnus Henoch

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

(defun jabber-private-get (node-name namespace success-callback error-callback)
  "Retrieve an item from private XML storage.
The item to retrieve is identified by NODE-NAME (a symbol) and
NAMESPACE (a string).
On success, SUCCESS-CALLBACK is called with the retrieved XML fragment.
On error, ERROR-CALLBACK is called with the entire IQ result."
  (jabber-send-iq nil "get"
		  `(query ((xmlns . "jabber:iq:private"))
			  (,node-name ((xmlns . ,namespace))))
		  #'jabber-private-get-1 success-callback
		  #'(lambda (xml-data error-callback)
		      (funcall error-callback xml-data))
		  error-callback))

(defun jabber-private-get-1 (xml-data success-callback)
  (funcall success-callback
	   (car (jabber-xml-node-children
		 (jabber-iq-query xml-data)))))

(defun jabber-private-set (fragment &optional 
				    success-callback success-closure-data
				    error-callback error-closure-data)
  "Store FRAGMENT in private XML storage.
SUCCESS-CALLBACK, SUCCESS-CLOSURE-DATA, ERROR-CALLBACK and
ERROR-CLOSURE-DATA are used as in `jabber-send-iq'."
  (jabber-send-iq nil "set"
		  `(query ((xmlns . "jabber:iq:private"))
			  ,fragment)
		  success-callback success-closure-data
		  error-callback error-closure-data))

(provide 'jabber-private)

;; arch-tag: 065bd03e-40fa-11da-ab48-000a95c2fcd0
