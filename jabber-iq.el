;; jabber-iq.el - infoquery functions
;; $Id: jabber-iq.el,v 1.2 2004/03/02 13:08:25 legoscia Exp $

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

(require 'jabber-core)

(defvar *jabber-open-info-queries* nil
  "an alist of open query id and their callback functions")

(defvar jabber-iq-get-xmlns-alist nil
  "Mapping from XML namespace to handler for IQ GET requests.")

(defvar jabber-iq-set-xmlns-alist nil
  "Mapping from XML namespace to handler for IQ SET requests.")

(defun jabber-browse-mode ()
"\\{jabber-browse-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'jabber-browse-mode
        mode-name "jabber-browse")
  (use-local-map jabber-browse-mode-map)
  (setq buffer-read-only t))

(put 'jabber-browse-mode 'mode-class 'special)

(defvar jabber-browse-mode-map (copy-keymap jabber-roster-mode-map))

(add-to-list 'jabber-iq-chain 'jabber-process-iq)
(defun jabber-process-iq (xml-data)
  "process an incoming iq stanza"
  (let* ((id (jabber-xml-get-attribute xml-data 'id))
         (type (jabber-xml-get-attribute xml-data 'type))
         (from (jabber-xml-get-attribute xml-data 'from))
	 (query (jabber-iq-query xml-data))
         (callback (cdr (assoc id *jabber-open-info-queries*))))
    (cond
     ;; if type is "result" or "error", this is a response to a query we sent.
     ((string= type "result")
      (let ((callback-cons (nth 0 callback)))
	(if (consp callback-cons)
	    (funcall (car callback-cons) xml-data (cdr callback-cons)))))
     ((string= type "error")
      (let ((callback-cons (nth 1 callback)))
	(if (consp callback-cons)
	    (funcall (car callback-cons) xml-data (cdr callback-cons)))))

     ;; if type is "get" or "set", correct action depends on namespace of request.
     ((and (listp query)
	   (string= type "get"))
      (let ((handler (cdr (assoc (jabber-xml-get-attribute query 'xmlns) jabber-iq-get-xmlns-alist))))
	(if handler
	    (funcall handler xml-data)
	  (jabber-send-sexp `(iq ((to . ,from)
				  (type . "error")
				  (id . ,id))
				 (error ((type . "cancel"))
					(feature-not-implemented
					 ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))))))))
     ((and (listp query)
	   (string= type "set")
      (let ((handler (cdr (assoc (jabber-xml-get-attribute query 'xmlns) jabber-iq-set-xmlns-alist))))
	(if handler
	    (funcall handler xml-data)
	  (jabber-send-sexp `(iq ((to . ,from)
				  (type . "error")
				  (id . ,id))
				 (error ((type . "cancel"))
					(feature-not-implemented
					 ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))))))))))))

(defun jabber-send-iq (to type query success-callback success-closure-data
			  error-callback error-closure-data &optional result-id)
  "Send an iq stanza to the specified entity, and optionally set up a callback.
TO is the addressee.
TYPE is one of \"get\", \"set\", \"result\" or \"error\".
QUERY is a list containing the child of the iq node in the format `jabber-sexp2xml'
accepts.
SUCCESS-CALLBACK is the function to be called when a successful result arrives.
SUCCESS-CLOSURE-DATA is the second argument to SUCCESS-CALLBACK.
ERROR-CALLBACK is the function to be called when an error arrives.
ERROR-CLOSURE-DATA is the second argument to ERROR-CALLBACK.
RESULT-ID is the id to be used for a response to a received iq message.
`jabber-report-success' and `jabber-process-data' are common callbacks."
  (let ((id (or result-id (apply 'format "emacs-iq-%d.%d.%d" (current-time)))))
    (if (or success-callback error-callback)
	(setq *jabber-open-info-queries* (cons (list id 
						     (cons success-callback success-closure-data)
						     (cons error-callback error-closure-data))

					       *jabber-open-info-queries*)))
    (jabber-send-sexp (list 'iq (append 
				 (if to (list (cons 'to to)))
				 (list (cons 'type type))
				 (list (cons 'id id)))
			    query))))

(defun jabber-process-data (xml-data closure-data)
  "Process random results from various requests."
  (let ((from (or (jabber-xml-get-attribute xml-data 'from) jabber-server))
	(xmlns (jabber-iq-xmlns xml-data))
	(type (jabber-xml-get-attribute xml-data 'type)))
    (with-current-buffer (get-buffer-create (concat "*-jabber-browse-:-" from "-*"))
      (if (not (eq major-mode 'jabber-browse-mode))
	  (jabber-browse-mode))

      (setq buffer-read-only nil)
      (goto-char (point-max))

      (insert (jabber-propertize from
			  'face 'jabber-title-large) "\n\n")

      ;; If closure-data is a function, call it.  If it is a string,
      ;; output it along with a description of the error.  For other
      ;; values (e.g. nil), just dump the XML.
      (cond
       ((functionp closure-data)
	(funcall closure-data xml-data))
       ((stringp closure-data)
	(insert closure-data ": " (jabber-parse-error (jabber-iq-error xml-data)) "\n\n"))
       (t
	(insert (format "%S\n\n" xml-data))))

      (run-hook-with-args 'jabber-alert-info-message-hooks 'browse (current-buffer) (funcall jabber-alert-info-message-function 'browse (current-buffer))))))

(provide 'jabber-iq)
