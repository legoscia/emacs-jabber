;; jabber-menu.el - menu definitions

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

(defvar jabber-jid-chat-menu nil
  "Menu items for chat menu")

(defvar jabber-jid-info-menu nil
  "Menu item for info menu")

(defvar jabber-jid-roster-menu nil
  "Menu items for roster menu")

(defvar jabber-jid-muc-menu nil
  "Menu items for MUC menu")

(defvar jabber-jid-service-menu nil
  "Menu items for service menu")

(defun jabber-popup-menu (which-menu)
  "Popup specified menu"
  (let* ((mouse-event (and (listp last-input-event) last-input-event))
	 (choice (widget-choose "Actions" which-menu mouse-event)))
    (if mouse-event
	(mouse-set-point mouse-event))
    (if choice
	(call-interactively choice))))

(defun jabber-popup-chat-menu ()
  "Popup chat menu"
  (interactive)
  (jabber-popup-menu jabber-jid-chat-menu))

(defun jabber-popup-info-menu ()
  "Popup info menu"
  (interactive)
  (jabber-popup-menu jabber-jid-info-menu))

(defun jabber-popup-roster-menu ()
  "Popup roster menu"
  (interactive)
  (jabber-popup-menu jabber-jid-roster-menu))

(defun jabber-popup-muc-menu ()
  "Popup MUC menu"
  (interactive)
  (jabber-popup-menu jabber-jid-muc-menu))

(defun jabber-popup-service-menu ()
  "Popup service menu"
  (interactive)
  (jabber-popup-menu jabber-jid-service-menu))

(defun jabber-popup-combined-menu ()
  "Popup combined menu"
  (interactive)
  (jabber-popup-menu (append jabber-jid-chat-menu jabber-jid-info-menu jabber-jid-roster-menu jabber-jid-muc-menu)))

(provide 'jabber-menu)

;;; arch-tag: 5147f52f-de47-4348-86ff-b799d7a75e3f
