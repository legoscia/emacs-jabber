;; jabber-time.el - time reporting by JEP-0090

;; Copyright (C) 2006 - Kirill A. Kroinskiy - catap@catap.ru

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'jabber-iq)
(require 'jabber-util)


(add-to-list 'jabber-jid-info-menu
	     (cons "Request time" 'jabber-get-tiem))
(defun jabber-get-time (to)
  "Request time"
  (interactive (list (jabber-read-jid-completing "Request time of: ")))
  (jabber-send-iq to
		  "get"
		  '(query ((xmlns . "jabber:iq:time")))
		  #'jabber-process-data #'jabber-process-time
		  #'jabber-process-data "Time request failed"))

;; called by jabber-process-data
(defun jabber-process-time (xml-data)
  "Handle results from jabber:iq:time requests."
  (let ((query (jabber-iq-query xml-data)))
    (let ((display 
	   (car (jabber-xml-node-children
		 (car (jabber-xml-get-children 
		       query 'display)))))
	  (utc
	   (car (jabber-xml-node-children
		 (car (jabber-xml-get-children 
		       query 'utc)))))
	  (tz
	   (car (jabber-xml-node-children
		 (car (jabber-xml-get-children 
		       query 'tz))))))
      (insert "Time:\t\t")
      (cond
       (display
	(insert display))
       (utc
	(insert (format-time-string "%Y-%m-%d %T" (jabber-parse-legacy-time utc)))))
      (insert "\n")
      (when tz
	(insert "Time zone:\t" tz)))))

(add-to-list 'jabber-iq-get-xmlns-alist (cons "jabber:iq:time" 'jabber-return-time))
(add-to-list 'jabber-advertised-features "jabber:iq:time")
(defun jabber-return-time (xml-data)
  "Return client time as defined in JEP-0090.  Sender and ID are
determined from the incoming packet passed in XML-DATA."
  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id)))
    (jabber-send-iq to "result"
		    `(query ((xmlns . "jabber:iq:time"))
			    (display () ,(format-time-string "%a %b %d %H:%M:%S %Y"))
			    (tz () ,(format-time-string "%Z"))
			    (utc () ,(jabber-encode-legacy-time nil)))
		    nil nil nil nil
		    id)))

(provide 'jabber-time)

;; arch-tag: 5396bfda-323a-11db-ac8d-000a95c2fcd0
