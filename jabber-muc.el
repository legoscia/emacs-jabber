;; jabber-muc.el - advanced MUC functions

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


;; Basic MUC/groupchat functionality is handled in jabber-chat.el.

(require 'jabber-widget)

(require 'cl)

(defvar *jabber-active-groupchats* nil
  "alist of groupchats and nicknames
Keys are strings, the bare JID of the room.
Values are strings.")

(defvar jabber-muc-participants nil
  "alist of groupchats and participants
Keys are strings, the bare JID of the room.
Values are lists of nickname strings.")

(defun jabber-muc-add-groupchat (group nickname)
  "Remember participating in GROUP under NICKNAME."
  (let ((whichgroup (assoc group *jabber-active-groupchats*)))
    (if whichgroup
	(setcdr whichgroup nickname)
      (add-to-list '*jabber-active-groupchats* (cons group nickname)))))

(defun jabber-muc-remove-groupchat (group)
  "Remove GROUP from internal bookkeeping."
  (let ((whichgroup (assoc group *jabber-active-groupchats*))
	(whichparticipants (assoc group jabber-muc-participants)))
    (setq *jabber-active-groupchats* 
	  (delq whichgroup *jabber-active-groupchats*))
    (setq jabber-muc-participants
	  (delq whichparticipants jabber-muc-participants))))

(defun jabber-muc-participant-plist (group nickname)
  "Return plist associated with NICKNAME in GROUP.
Return nil if nothing known about that combination."
  (let ((whichparticipants (assoc group jabber-muc-participants)))
    (when whichparticipants
      (cdr (assoc nickname whichparticipants)))))

(defun jabber-muc-modify-participant (group nickname new-plist)
  "Assign properties in NEW-PLIST to NICKNAME in GROUP."
  (let ((participants (assoc group jabber-muc-participants)))
    ;; either we have a list of participants already...
    (if participants
	(let ((participant (assoc nickname participants)))
	  ;; and maybe this participant is already in the list
	  (if participant
	      ;; if so, just update role, affiliation, etc.
	      ;; XXX: calculate delta and report to user? e.g. "X was given voice"
	      (setf (cdr participant) new-plist)
	    (push (cons nickname new-plist) (cdr participants))))
      ;; or we don't
      (push (cons group (list (cons nickname new-plist))) jabber-muc-participants))))

(defun jabber-muc-remove-participant (group nickname)
  "Forget everything about NICKNAME in GROUP."
  (let ((participants (assoc group jabber-muc-participants)))
    (when participants
      (let ((participant (assoc nickname (cdr participants))))
	(setf (cdr participants) (delq participant (cdr participants)))))))

(defun jabber-muc-read-completing (prompt)
  "Read the name of a joined chatroom."
  (jabber-read-jid-completing prompt
			      (if (null *jabber-active-groupchats*)
				  (error "You haven't joined any group")
				(mapcar (lambda (x) (jabber-jid-symbol (car x)))
					*jabber-active-groupchats*))
			      t
			      jabber-group))

(defun jabber-muc-read-nickname (group prompt)
  "Read the nickname of a participant in GROUP."
  (let ((nicknames (cdr (assoc group jabber-muc-participants))))
    (unless nicknames
      (error "Unknown group: %s" group))
    (completing-read prompt nicknames nil t)))

(add-to-list 'jabber-jid-muc-menu
   (cons "Configure groupchat" 'jabber-groupchat-get-config))
(defun jabber-groupchat-get-config (group)
  "Ask for MUC configuration form"
  (interactive (list (jabber-muc-read-completing "Configure group: ")))
  (jabber-send-iq group
		  "get"
		  '(query ((xmlns . "http://jabber.org/protocol/muc#owner")))
		  #'jabber-process-data #'jabber-groupchat-render-config
		  #'jabber-process-data "MUC configuration request failed"))

(defun jabber-groupchat-render-config (xml-data)
  "Render MUC configuration form"

  (let ((query (jabber-iq-query xml-data))
	xdata)
    (dolist (x (jabber-xml-get-children query 'x))
      (if (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data")
	  (setq xdata x)))
    (if (not xdata)
	(insert "No configuration possible.\n")
      
    (jabber-init-widget-buffer (jabber-xml-get-attribute xml-data 'from))

    (jabber-render-xdata-form xdata)

    (widget-create 'push-button :notify #'jabber-groupchat-submit-config "Submit")
    (widget-insert "\t")
    (widget-create 'push-button :notify #'jabber-groupchat-cancel-config "Cancel")
    (widget-insert "\n")

    (widget-setup)
    (widget-minor-mode 1))))

(defun jabber-groupchat-submit-config (&rest ignore)
  "Submit MUC configuration form."

  (jabber-send-iq jabber-submit-to
		  "set"
		  `(query ((xmlns . "http://jabber.org/protocol/muc#owner"))
			  ,(jabber-parse-xdata-form))
		  #'jabber-report-success "MUC configuration"
		  #'jabber-report-success "MUC configuration"))

(defun jabber-groupchat-cancel-config (&rest ignore)
  "Cancel MUC configuration form."

  (jabber-send-iq jabber-submit-to
		  "set"
		  '(query ((xmlns . "http://jabber.org/protocol/muc#owner"))
			  (x ((xmlns . "jabber:x:data") (type . "cancel"))))
		  nil nil nil nil))

(add-to-list 'jabber-jid-muc-menu
	     (cons "Join groupchat" 'jabber-groupchat-join))

(defun jabber-groupchat-join (group nickname)
  "join a groupchat"
  (interactive (list (jabber-read-jid-completing "group: ")
		     (jabber-read-with-input-method (format "Nickname: (default %s) "
							    jabber-nickname) 
						    nil nil jabber-nickname)))
  (jabber-send-sexp `(presence ((to . ,(format "%s/%s" group nickname)))
			       (x ((xmlns . "http://jabber.org/protocol/muc")))))

  (let ((whichgroup (assoc group *jabber-active-groupchats*)))
    (if whichgroup
	(setcdr whichgroup nickname)
      (add-to-list '*jabber-active-groupchats* (cons group nickname))))
  
  (jabber-groupchat-display group))

(add-to-list 'jabber-jid-muc-menu
	     (cons "Leave groupchat" 'jabber-groupchat-leave))

(defun jabber-groupchat-leave (group)
  "leave a groupchat"
  (interactive (list (jabber-muc-read-completing "Leave which group: ")))
  (let ((whichgroup (assoc group *jabber-active-groupchats*)))
    ;; send unavailable presence to our own nick in room
    (jabber-send-sexp `(presence ((to . ,(format "%s/%s" group (cdr whichgroup)))
				  (type . "unavailable"))))))

(add-to-list 'jabber-jid-muc-menu
	     (cons "List participants" 'jabber-muc-names))

(defun jabber-muc-names (group)
  "Print names, affiliations, and roles of participants in GROUP."
  (interactive (list (jabber-muc-read-completing "Group: ")))
  (jabber-groupchat-display 
   group nil
   (apply 'concat "Participants:\n"
	  (format "%-15s %-15s %-11s %s\n" "Nickname" "Role" "Affiliation" "JID")
	  (mapcar (lambda (x)
		    (let ((plist (cdr x)))
		      (format "%-15s %-15s %-11s %s\n"
			      (car x)
			      (plist-get plist 'role)
			      (plist-get plist 'affiliation)
			      (or (plist-get plist 'jid) ""))))
		  (cdr (assoc group jabber-muc-participants))))))

(add-to-list 'jabber-jid-muc-menu
	     (cons "Set role (kick, voice, op)" 'jabber-muc-set-role))

(defun jabber-muc-set-role (group nickname role reason)
  "Set role of NICKNAME in GROUP to ROLE, specifying REASON."
  (interactive
   (let* ((group (jabber-muc-read-completing "Group: "))
	  (nickname (jabber-muc-read-nickname group "Nickname: ")))
     (list group nickname
	   (completing-read "New role: " '("none" "visitor" "participant" "moderator") nil t)
	   (read-string "Reason: "))))
  (unless (or (zerop (length nickname)) (zerop (length role)))
    (jabber-send-iq group "set"
		    `(query ((xmlns . "http://jabber.org/protocol/muc#admin"))
			    (item ((nick . ,nickname)
				   (role . ,role))
				  ,(unless (zerop (length reason))
				     `(reason () ,reason))))
		    'jabber-report-success "Role change"
		    'jabber-report-success "Role change")))

(defun jabber-muc-message-p (message)
  "Return non-nil if MESSAGE is a groupchat message.
That does not include private messages in a groupchat."
  ;; Public groupchat messages have type "groupchat" and are from
  ;; room@server/nick.  Public groupchat errors have type "error" and
  ;; are from room@server.
  (let ((from (jabber-xml-get-attribute message 'from))
	(type (jabber-xml-get-attribute message 'type)))
    (or 
     (string= type "groupchat")
     (and (string= type "error")
	  (assoc from *jabber-active-groupchats*)))))

(defun jabber-muc-presence-p (presence)
  "Return non-nil if PRESENCE is presence from groupchat."
  (let ((from (jabber-xml-get-attribute presence 'from)))
    (assoc (jabber-jid-user from) *jabber-active-groupchats*)))

(defun jabber-muc-parse-affiliation (x-muc)
  "Parse X-MUC in the muc#user namespace and return a plist.
Return nil if X-MUC is nil."
  ;; XXX: parse <actor/> and <reason/> tags?  or maybe elsewhere?
  (apply 'nconc (mapcar (lambda (prop) (list (car prop) (cdr prop)))
			(jabber-xml-node-attributes
			 (car (jabber-xml-get-children x-muc 'item))))))

(defun jabber-muc-process-presence (presence)
  (let ((from (jabber-xml-get-attribute presence 'from))
	(type (jabber-xml-get-attribute presence 'type))
	(x-muc (find-if 
		(lambda (x) (equal (jabber-xml-get-attribute x 'xmlns)
				   "http://jabber.org/protocol/muc#user"))
		(jabber-xml-get-children presence 'x))))
    (let ((group (jabber-jid-user from))
	  (nickname (jabber-jid-resource from))
	  (symbol (jabber-jid-symbol from)))
      ;; handle leaving a room
      (cond 
       ((string= type "unavailable")
	;; are we leaving?
	(if (string= nickname (cdr (assoc group *jabber-active-groupchats*)))
	    (progn
	      (jabber-muc-remove-groupchat group)
	      (jabber-groupchat-display group nil "You have left the chatroom"))
	  ;; or someone else?
	  (jabber-muc-remove-participant group nickname)
	  (jabber-groupchat-display group nil (format "%s has left the chatroom" nickname))))
       ;; XXX: add errors here
       (t 
	;; someone is entering
	(let ((new-participant (not (jabber-muc-participant-plist group nickname)))
	      (new-plist (jabber-muc-parse-affiliation x-muc)))
	  (jabber-muc-modify-participant group nickname new-plist)
	  (when new-participant
	    (jabber-groupchat-display group nil (format "%s enters the chatroom" nickname)))))))))
	      
(provide 'jabber-muc)

;;; arch-tag: 1ff7ab35-1717-46ae-b803-6f5b3fb2cd7d
