;; jabber.el - a minimal jabber client
;; $Id: jabber.el,v 1.38 2004/02/25 21:42:02 legoscia Exp $

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

(require 'xml)
(require 'sha1-el)
(require 'widget)

;;; load Unicode tables
(if (featurep 'xemacs)
    (require 'un-define))

(eval-when-compile
  (require 'wid-edit))

;;; guess internal dependencies!
(require 'jabber-util)
(require 'jabber-xml)
(require 'jabber-core)
(require 'jabber-roster)
(require 'jabber-presence)
(require 'jabber-alert)
(require 'jabber-chat)
(require 'jabber-disco)
(require 'jabber-iq)

(defvar *jabber-active-groupchats* nil
  "a list of the groupchats we are currently in")

(defvar *jabber-current-status* ""
  "the users current presence staus")

(defvar *jabber-current-show* ""
  "the users current presence show")

(defvar *jabber-current-priority* nil
  "the user's current priority")

(defvar *jabber-status-history* nil
  "history of status messages")

(defvar jabber-chatting-with nil
  "JID of the person you are chatting with")

(defvar jabber-group nil
  "the groupchat you are participating in")

(defvar jabber-widget-alist nil
  "Alist of widgets currently used")

(defvar jabber-form-type nil
  "Type of form.  One of:
'x-data, jabber:x:data
'register, as used in jabber:iq:register and jabber:iq:search")

(defvar jabber-submit-to nil
  "JID of the entity to which form data is to be sent")

(defgroup jabber nil "Jabber instant messaging"
  :group 'emacs)

(defgroup jabber-faces nil "faces for displaying jabber instant messaging"
  :group 'jabber)

(defface jabber-title-small
  '((t (:weight bold :width semi-expanded :height 1.0)))
  "face for small titles"
  :group 'jabber-faces)

(defface jabber-title-medium
  '((t (:weight bold :width expanded :height 2.0)))
  "face for medium titles"
  :group 'jabber-faces)

(defface jabber-title-large
  '((t (:weight bold :width ultra-expanded :height 3.0)))
  "face for large titles"
  :group 'jabber-faces)

(defface jabber-roster-user-online
  '((t (:foreground "blue" :weight bold :slant normal)))
  "face for displaying online users"
  :group 'jabber-faces)

(defface jabber-roster-user-xa
  '((t (:foreground "black" :weight normal :slant italic)))
  "face for displaying extended away users"
  :group 'jabber-faces)

(defface jabber-roster-user-dnd
  '((t (:foreground "red" :weight normal :slant italic)))
  "face for displaying do not disturb users"
  :group 'jabber-faces)

(defface jabber-roster-user-away
  '((t (:foreground "dark green" :weight normal :slant italic)))
  "face for displaying away users"
  :group 'jabber-faces)

(defface jabber-roster-user-chatty
  '((t (:foreground "dark orange" :weight bold :slant normal)))
  "face for displaying chatty users"
  :group 'jabber-faces)

(defface jabber-roster-user-error
  '((t (:foreground "red" :weight light :slant italic)))
  "face for displaying users sending presence errors"
  :group 'jabber-faces)

(defface jabber-roster-user-offline
  '((t (:foreground "dark grey" :weight light :slant italic)))
  "face for displaying offline users"
  :group 'jabber-faces)

(defface jabber-chat-prompt-local
  '((t (:foreground "blue" :weight bold)))
  "face for displaying the chat prompt for what you type in"
  :group 'jabber-faces)

(defface jabber-chat-prompt-foreign
  '((t (:foreground "red" :weight bold)))
  "face for displaying the chat prompt for what they send"
  :group 'jabber-faces)

(defcustom jabber-debug nil 
  "show debugging information." 
  :type 'boolean
  :group 'jabber)

(defcustom jabber-username "emacs"
  "jabber username (user part of JID)" 
  :type 'string
  :group 'jabber)

(defcustom jabber-server "magaf.org" 
  "jabber server (domain part of JID)" 
  :type 'string
  :group 'jabber)

(defcustom jabber-network-server nil
  "hostname or IP address of server to connect to, if different from `jabber-server'."
  :type '(radio (const :tag "Same as `jabber-server'" nil)
		(string :tag "Hostname or IP address"))
  :group 'jabber)

(defcustom jabber-port 5222
  "jabber port" 
  :type 'integer
  :group 'jabber)

(defcustom jabber-password nil
  "jabber password" 
  :type '(radio (const :tag "Prompt for password" nil)
		 (string :tag "Save password in .emacs"))
  :group 'jabber)

(defcustom jabber-resource "emacs"
  "jabber resource" 
  :type 'string
  :group 'jabber)

(defcustom jabber-default-priority 10
  "default priority"
  :type 'integer
  :group 'jabber)

(defcustom jabber-nickname "emacs"
  "jabber groupchat nickname" 
  :type 'string
  :group 'jabber)

(defcustom jabber-sort-order '("chat" "" "away" "dnd" "xa")
  "Sort by status in this order.  Anything not in list goes last.
Offline is represented as nil."
  :type '(repeat (restricted-sexp :match-alternatives (stringp nil)))
  :group 'jabber)

(defcustom jabber-show-resources 'sometimes
  "Show resources in roster?"
  :type '(radio (const :tag "Never" nil)
		(const :tag "When more than one connected resource" sometimes)
		(const :tag "Always" always))
  :group 'jabber)

(define-key global-map
  [menu-bar jabber-menu]
  (cons "Jabber" (make-sparse-keymap "jabber-menu")))

(define-key global-map
  [menu-bar jabber-menu jabber-menu-connect]
  '("Connect" . jabber-connect))

(define-key global-map
  [menu-bar jabber-menu jabber-menu-disconnect]
  '("Disconnect" . jabber-disconnect))

(define-key global-map
  [menu-bar jabber-menu jabber-menu-browse]
  '("Browse" . jabber-get-browse))

(define-key global-map
  [menu-bar jabber-menu jabber-menu-customize]
  '("Customize" . jabber-customize))

(define-key global-map
  [menu-bar jabber-menu jabber-menu-status]
  (cons "Set Status" (make-sparse-keymap "set-status")))

(defconst jabber-presence-faces
 '(("" . jabber-roster-user-online)
   ("away" . jabber-roster-user-away)
   ("xa" . jabber-roster-user-xa)
   ("dnd" . jabber-roster-user-dnd)
   ("chat" . jabber-roster-user-chatty)
   ("error" . jabber-roster-user-error)
   (nil . jabber-roster-user-offline))
 "Mapping from presence types to faces")

(defconst jabber-presence-strings
  '(("" . "Online")
    ("away" . "Away")
    ("xa" . "Extended Away")
    ("dnd" . "Do not Disturb")
    ("chat" . "Chatty")
    ("error" . "Error")
    (nil . "Offline"))
  "Mapping from presence types to readable strings")

(defmacro jabber-define-status-key (title show)
  (list 'let (list ( list 'func (list 'make-symbol (list 'concat "jabber-send-presence-" show)))
         (list 'menu-item (list 'make-symbol (list 'concat "jabber-menu-status-" show))))
     (list 'fset 'func `(lambda () (interactive)
                           (jabber-send-presence ,show
						 (jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*)
						 (format "%d" *jabber-current-priority*))))
     (list 'define-key 'global-map
           (list 'vector ''menu-bar ''jabber-menu ''jabber-menu-status 'menu-item)
           (list 'cons title 'func))))

;;;(dolist (presence jabber-presence-strings)
;;;  (jabber-define-status-key (cdr presence) (car presence)))
(jabber-define-status-key "Online" "")
(jabber-define-status-key "Chatty" "chat")
(jabber-define-status-key "Away" "away")
(jabber-define-status-key "Extended Away" "xa")
(jabber-define-status-key "Do not Disturb" "dnd")






(defconst jabber-jid-chat-menu
  (list
   (cons "Start chat" 'jabber-chat-with)
   (cons "Send message" 'jabber-send-message))
  "Menu items for chat menu")

(defconst jabber-jid-info-menu
  (list
   (cons "Send items disco query" 'jabber-get-disco-items)
   (cons "Send info disco query" 'jabber-get-disco-info)
   (cons "Send browse query" 'jabber-get-browse)
   (cons "Request software version" 'jabber-get-version))
  "Menu item for info menu")

(defconst jabber-jid-roster-menu
  (list
   (cons "Send subscription request" 'jabber-send-subscription-request)
   (cons "Add/modify roster entry" 'jabber-roster-change)
   (cons "Delete roster entry" 'jabber-roster-delete))
  "Menu items for roster menu")

(defconst jabber-jid-muc-menu
  (list
   (cons "Join groupchat" 'jabber-groupchat-join)
   (cons "Leave groupchat" 'jabber-groupchat-leave)
   (cons "Configure groupchat" 'jabber-groupchat-get-config))
  "Menu items for MUC menu")

(defconst jabber-jid-service-menu
  (list
   (cons "Register with service" 'jabber-get-register)
   (cons "Search directory" 'jabber-get-search))
  "Menu items for service menu")

(defconst jabber-jid-menu
  (append jabber-jid-chat-menu jabber-jid-info-menu jabber-jid-roster-menu jabber-jid-muc-menu)
  "All menu items")



(defun jabber-customize ()
  "customize jabber options"
  (interactive)
  (customize-group 'jabber))

(cond
 ((fboundp 'replace-in-string)
  (defsubst jabber-replace-in-string (str regexp newtext)
    (replace-in-string str regexp newtext t)))
 ((fboundp 'replace-regexp-in-string)
  (defsubst jabber-replace-in-string (str regexp newtext)
    (replace-regexp-in-string regexp newtext str t t))))











;;; XXX: include nicknames




(defun jabber-browse-mode ()
"\\{jabber-browse-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'jabber-browse-mode
        mode-name "jabber-browse")
  (use-local-map jabber-browse-mode-map)
  (setq buffer-read-only t))

(put 'jabber-browse-mode 'mode-class 'special)

(defvar jabber-browse-mode-map nil)

(unless jabber-browse-mode-map
  (setq jabber-browse-mode-map jabber-roster-mode-map)
)

(defun jabber-groupchat-mode ()
  "\\{jabber-groupchat-mode-map}"
  (kill-all-local-variables)
  (make-local-variable 'jabber-group)
  (setq major-mode 'jabber-groupchat-mode
        mode-name "jabber-groupchat")
  (use-local-map jabber-groupchat-mode-map)
  (setq buffer-read-only t))

(put 'jabber-groupchat-mode 'mode-class 'special)

(defvar jabber-groupchat-mode-map (make-keymap))

(suppress-keymap jabber-groupchat-mode-map)

(dolist (key (append "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890`~!@#$%^&*()_+-=[]{}|';:/?.,>< " nil))
  (let ((send-fun (make-symbol (concat "jabber-groupchat-buffer-send-" (char-to-string key)))))
    (fset send-fun `(lambda (body) (interactive (list (jabber-read-with-input-method "" ,(char-to-string key))))
		      (jabber-send-groupchat jabber-group body)
		      (setq buffer-read-only nil)
		      (goto-char (point-max))
		      (if (not (eq major-mode 'jabber-groupchat-mode))
			  (jabber-groupchat-mode))))
    (define-key jabber-groupchat-mode-map (char-to-string key) send-fun)))

(defun jabber-groupchat-display (group &optional nick body timestamp)
  "display the groupchat window and an incoming message, if there is one.
TIMESTAMP is timestamp, or nil for now."
  (with-current-buffer (get-buffer-create (concat "*-jabber-groupchat-:-" group "-*"))
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (if body (insert (jabber-propertize (concat "[" (substring (current-time-string timestamp) 11 16) "] " nick)
                                 'face 'jabber-chat-prompt-foreign)
                     "> " body "\n"))
    (if (not (eq major-mode 'jabber-groupchat-mode))
	(jabber-groupchat-mode))
    (setq jabber-group group)
    (run-hook-with-args 'jabber-alert-message-hooks group (current-buffer) body (funcall jabber-alert-message-function group (current-buffer) body))))














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
  (jabber-popup-menu jabber-jid-menu))





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

(defun jabber-process-search-result (xml-data)
  "Receive and display search results."

  ;; This function assumes that all search results come in one packet,
  ;; which is not necessarily the case.
  (let ((query (jabber-iq-query xml-data))
	(have-xdata nil)
	xdata fields (jid-fields 0))

    ;; First, check for results in jabber:x:data form.
    (dolist (x (jabber-xml-get-children query 'x))
      (when (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data")
	(setq have-xdata t)
	(setq xdata x)))

    (if have-xdata
	(let ((title (car (jabber-xml-get-children xdata 'title))))
	  (when title
	    (insert (jabber-propertize (car (jabber-xml-node-children title)) 'face 'jabber-title-medium) "\n")))
      (insert (jabber-propertize "Search results" 'face 'jabber-title-medium) "\n"))
	
    (if have-xdata
	(let ((reported (car (jabber-xml-get-children xdata 'reported)))
	      (column 0))
	  (dolist (field (jabber-xml-get-children reported 'field))
	    (let (width)
	      ;; Clever algorithm for estimating width based on field type goes here.
	      (setq width 20)

	      (setq fields
		    (append
		     fields
		     (list (cons (jabber-xml-get-attribute field 'var)
				 (list 'label (jabber-xml-get-attribute field 'label)
				       'type (jabber-xml-get-attribute field 'type)
				       'column column)))))
	      (setq column (+ column width))
	      (if (string= (jabber-xml-get-attribute field 'type) "jid-single")
		  (setq jid-fields (1+ jid-fields))))))
      (setq fields '((first . (label "First name" column 0))
		     (last . (label "Last name" column 15))
		     (nick . (label "Nickname" column 30))
		     (jid . (label "JID" column 45))
		     (email . (label "E-mail" column 65))))
      (setq jid-fields 1))

    (dolist (field-cons fields)
      (indent-to (plist-get (cdr field-cons) 'column) 1)
      (insert (jabber-propertize (plist-get (cdr field-cons) 'label) 'face 'bold)))
    (insert "\n\n")

    ;; Now, the items
    (dolist (item (if have-xdata
		      (jabber-xml-get-children xdata 'item)
		    (jabber-xml-get-children query 'item)))
      (let ((start-of-line (point))
	    jid)

	(if have-xdata
	      ;; The following code assumes that the order of the <field/>s in each
	      ;; <item/> is the same as in the <reported/> tag.
	      (dolist (field (jabber-xml-get-children item 'field))
		(let ((field-plist (cdr (assoc (jabber-xml-get-attribute field 'var) fields)))
		      (value (car (jabber-xml-node-children (car (jabber-xml-get-children field 'value))))))

		  (indent-to (plist-get field-plist 'column) 1)

		  ;; If there is only one JID field, let the whole row have the jabber-jid
		  ;; property.  If there are many JID fields, the string belonging to each
		  ;; field has that property.
		  (if (string= (plist-get field-plist 'type) "jid-single")
		      (if (not (eq jid-fields 1))
			  (insert (jabber-propertize value 'jabber-jid value))
			(setq jid value)
			(insert value))
		    (insert value))))

	  (dolist (field-cons fields)
	    (let ((field-plist (cdr field-cons))
		  (value (if (eq (car field-cons) 'jid) 
			     (setq jid (jabber-xml-get-attribute item 'jid))
			   (car (jabber-xml-node-children (car (jabber-xml-get-children item (car field-cons))))))))
	      (indent-to (plist-get field-plist 'column) 1)
	      (if value (insert value)))))
	      
	(if jid
	    (put-text-property start-of-line (point)
			       'jabber-jid jid))
	(insert "\n")))))

(defun jabber-process-browse (xml-data)
  "Handle results from jabber:iq:browse requests."
  (dolist (item (jabber-xml-node-children xml-data))
    (when (and (listp item)
	       (not (eq (jabber-xml-node-name item) 'ns)))
      (let ((jid (jabber-xml-get-attribute item 'jid))
	    (beginning (point)))
	(cond
	 ((or
	   (eq (jabber-xml-node-name item) 'user)
	   (string= (jabber-xml-get-attribute item 'category) "user"))
	  (insert (jabber-propertize "$ USER"
			      'face 'jabber-title-medium)
		  "\n\n"))
	 ((or
	   (eq (jabber-xml-node-name item) 'service)
	   (string= (jabber-xml-get-attribute item 'category) "service"))
	  (insert (jabber-propertize "* SERVICE"
			      'face 'jabber-title-medium)
		  "\n\n"))
	 ((or
	   (eq (jabber-xml-node-name item) 'conference)
	   (string= (jabber-xml-get-attribute item 'category) "conference"))
	  (insert (jabber-propertize "@ CONFERENCE"
			      'face 'jabber-title-medium)
		  "\n\n"))
	 (t
	  ;; So far I've seen "server" and "directory", both in the node-name.
	  ;; Those are actually service disco categories, but jabberd 2 seems
	  ;; to use them for browse results as well.  It's not right (as in
	  ;; JEP-0011), but it's reasonable.
	  (let ((category (jabber-xml-get-attribute item 'category)))
	    (if (= (length category) 0)
		(setq category (jabber-xml-node-name item)))
	    (insert (jabber-propertize (format "! OTHER: %s" category)
				'face 'jabber-title-medium)
		    "\n\n"))))
	(dolist (attr '((type . "Type:\t\t")
			(jid . "JID:\t\t")
			(name . "Name:\t\t")
			(version . "Version:\t")))
	  (let ((data (jabber-xml-get-attribute item (car attr))))
	    (if (> (length data) 0)
		(insert (cdr attr) (jabber-unescape-xml data) "\n"))))

	(dolist (ns (jabber-xml-get-children item 'ns))
	  (if (stringp (car (jabber-xml-node-children ns)))
	      (insert "Namespace:\t" (car (jabber-xml-node-children ns)) "\n")))


	(put-text-property beginning (point) 'jabber-jid jid)
	(insert "\n\n")

	;; XXX: Is this kind of recursion really needed?
	(if (listp (car (jabber-xml-node-children item)))
	    (jabber-process-browse item))))))



(defun jabber-process-version (xml-data)
  "Handle results from jabber:iq:version requests."
  
  (let ((query (jabber-iq-query xml-data)))
    (dolist (x '((name . "Name:\t\t") (version . "Version:\t") (os . "OS:\t\t")))
      (let ((data (car (jabber-xml-node-children (car (jabber-xml-get-children query (car x)))))))
	(when data
	  (insert (cdr x) data "\n"))))))

(defun jabber-render-register-form (query)
  "Display widgets from <query/> element in jabber:iq:{register,search} namespace."
  (make-local-variable 'jabber-widget-alist)
  (setq jabber-widget-alist nil)
  (make-local-variable 'jabber-form-type)
  (setq jabber-form-type 'register)

  (if (jabber-xml-get-children query 'instructions)
      (widget-insert "Instructions: " (car (jabber-xml-node-children (car (jabber-xml-get-children query 'instructions)))) "\n"))
  (if (jabber-xml-get-children query 'registered)
      (widget-insert "You are already registered.  You can change your details here.\n"))
  (widget-insert "\n")

  (let ((possible-fields
	 ;; taken from JEP-0077
	 '((username . "Username")
	   (nick . "Nickname")
	   (password . "Password")
	   (name . "Full name")
	   (first . "First name")
	   (last . "Last name")
	   (email . "E-mail")
	   (address . "Address")
	   (city . "City")
	   (state . "State")
	   (zip . "Zip")
	   (phone . "Telephone")
	   (url . "Web page")
	   (date . "Birth date"))))
    (dolist (field (jabber-xml-node-children query))
      (let ((entry (assq (jabber-xml-node-name field) possible-fields)))
	(when entry
	  (widget-insert (cdr entry) "\t")
	  (setq jabber-widget-alist 
		(cons
		 (cons (car entry)
		       (widget-create 'editable-field
				      :secret  (if (eq (car entry) 'password)
						   ?* nil)
				      (or (car (jabber-xml-node-children
						field)) "")))
		 jabber-widget-alist))
	  (widget-insert "\n"))))))

(defun jabber-parse-register-form ()
  "Return children of a <query/> tag containing information entered in the widgets of the current buffer."
  (mapcar
   (lambda (widget-cons)
     (list (car widget-cons)
	   nil
	   (widget-value (cdr widget-cons))))
   jabber-widget-alist))

(defun jabber-render-xdata-form (x)
  "Display widgets from <x/> element in jabber:x:data namespace."
  (make-local-variable 'jabber-widget-alist)
  (setq jabber-widget-alist nil)
  (make-local-variable 'jabber-form-type)
  (setq jabber-form-type 'xdata)

  (let ((title (car (jabber-xml-node-children (car (jabber-xml-get-children x 'title))))))
    (if (stringp title)
	(widget-insert (jabber-propertize title 'face 'jabber-title-medium) "\n\n")))
  (let ((instructions (car (jabber-xml-node-children (car (jabber-xml-get-children x 'instructions))))))
    (if (stringp instructions)
	(widget-insert "Instructions: " instructions "\n\n")))

  (dolist (field (jabber-xml-get-children x 'field))
    (let ((var (jabber-xml-get-attribute field 'var))
	  (label (jabber-xml-get-attribute field 'label))
	  (type (jabber-xml-get-attribute field 'type))
	  (required (jabber-xml-get-children field 'required))
	  (values (jabber-xml-get-children field 'value))
	  (options (jabber-xml-get-children field 'option))
	  (desc (car (jabber-xml-get-children field 'desc))))
      ;; "required" not implemented yet

      (cond
       ((string= type "fixed")
	(widget-insert (car (jabber-xml-node-children (car values)))))

       ((string= type "text-multi")
	(if (or label var)
	    (widget-insert (or label var) ":\n"))
	(push (cons (cons var type)
		    (widget-create 'text (or (car (jabber-xml-node-children (car values))) "")))
	      jabber-widget-alist))

       ((string= type "list-single")
	(if (or label var)
	    (widget-insert (or label var) ":\n"))
	(push (cons (cons var type)
		    (apply 'widget-create
			   'radio-button-choice 
			   :value (car (xml-node-children (car values)))
			   (mapcar (lambda (option)
				     `(item :tag ,(jabber-xml-get-attribute option 'label)
					    :value ,(car (jabber-xml-node-children (car (jabber-xml-get-children option 'value))))))
				   options)))
	      jabber-widget-alist))
				    
       ((string= type "boolean")
	(push (cons (cons var type)
		    (widget-create 'checkbox :tag (or label var) :value (not (string= (car (xml-node-children (car (values)))) "0"))))
	      jabber-widget-alist)
	(if (or label var)
	    (widget-insert " " (or label var) "\n")))

       (t				; in particular including text-single and text-private
	(if (or label var)
	    (widget-insert (or label var) ": "))
	(setq jabber-widget-alist
	      (cons
	       (cons (cons var type)
		     (widget-create 'editable-field
				    :secret (if (string= type "text-private") ?* nil)
				    (or (car (jabber-xml-node-children (car values)))
					"")))
	       jabber-widget-alist))))
      (when desc
	(widget-insert "\n" (car (jabber-xml-node-children desc))))
      (widget-insert "\n\n"))))

(defun jabber-parse-xdata-form ()
  "Return an <x/> tag containing information entered in the widgets of the current buffer."
  `(x ((xmlns . "jabber:x:data")
       (type . "submit"))
      ,@(mapcar
	 (lambda (widget-cons)
	   (let ((values (jabber-xdata-value-convert (widget-value (cdr widget-cons)) (cdar widget-cons))))
	     ;; empty fields are not included
	     (when values
	       `(field ((var . ,(caar widget-cons)))
		       ,@(mapcar
			  (lambda (value)
			    (list 'value nil value))
			  values)))))
	 jabber-widget-alist)))

(defun jabber-xdata-value-convert (value type)
  "Convert VALUE from form used by widget library to form required by JEP-0004.
Return a list of strings, each of which to be included as cdata in a <value/> tag."
  (cond
   ((string= type "boolean")
    (if value (list "1") (list "0")))
   ((string= type "text-multi")
    (split-string value "[\n\r]"))
   (t					; in particular including text-single, text-private and list-single
    (if (zerop (length value))
	nil
      (list value)))))

(defun jabber-init-widget-buffer (submit-to)
  "Setup buffer-local variables for widgets."
  (make-local-variable 'jabber-widget-alist)
  (make-local-variable 'jabber-submit-to)
  (setq jabber-widget-alist nil)
  (setq jabber-submit-to submit-to)
  (setq buffer-read-only nil)
  ;; XXX: This is because data from other queries would otherwise be
  ;; appended to this buffer, which would fail since widget buffers
  ;; are read-only... or something like that.  Maybe there's a
  ;; better way.
  (rename-uniquely))

(defun jabber-process-register-or-search (xml-data)
  "Display results from jabber:iq:{register,search} query as a form."

  (let ((query (jabber-iq-query xml-data))
	(have-xdata nil)
	(type (cond
	       ((string= (jabber-iq-xmlns xml-data) "jabber:iq:register")
		'register)
	       ((string= (jabber-iq-xmlns xml-data) "jabber:iq:search")
		'search)
	       (t
		(error "Namespace %s not handled by jabber-process-register-or-search" (jabber-iq-xmlns xml-data))))))
	       
    (cond
     ((eq type 'register)
      ;; If there is no `from' attribute, we are registering with the server
      (jabber-init-widget-buffer (or (jabber-xml-get-attribute xml-data 'from) jabber-server)))

     ((eq type 'search)
      ;; no such thing here
      (jabber-init-widget-buffer (jabber-xml-get-attribute xml-data 'from))))

    (widget-insert (if (eq type 'register) "Register with " "Search ") jabber-submit-to "\n")

    (dolist (x (jabber-xml-get-children query 'x))
      (when (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data")
	(setq have-xdata t)
	(jabber-render-xdata-form x)))
    (if (not have-xdata)
	(jabber-render-register-form query))

    (widget-create 'push-button :notify (if (eq type 'register)
					    #'jabber-submit-register
					  #'jabber-submit-search) "Submit")
    (when (eq type 'register)
      (widget-insert "\t")
      (widget-create 'push-button :notify #'jabber-remove-register "Cancel registration"))
    (widget-insert "\n")
    (widget-setup)
    (widget-minor-mode 1)))

(defun jabber-submit-register (&rest ignore)
  "Submit registration input.  See `jabber-process-register-or-search'."
  
  (let ((handler (if jabber-register-p 
		     #'jabber-process-register-secondtime
		   #'jabber-report-success))
	(text (concat "Registration with " jabber-submit-to)))
    (jabber-send-iq jabber-submit-to
		    "set"

		    (cond
		     ((eq jabber-form-type 'register)
		      `(query ((xmlns . "jabber:iq:register"))
			      ,@(jabber-parse-register-form)))
		     ((eq jabber-form-type 'xdata)
		      `(query ((xmlns . "jabber:iq:register"))
			      ,(jabber-parse-xdata-form)))
		     (t
		      (error "Unknown form type: %s" jabber-form-type)))
		    handler (if jabber-register-p 'success text)
		    handler (if jabber-register-p 'failure text)))

  (message "Registration sent"))

(defun jabber-submit-search (&rest ignore)
  "Submit search.  See `jabber-process-register-or-search'."
  
  (let ((text (concat "Search at " jabber-submit-to)))
    (jabber-send-iq jabber-submit-to
		    "set"

		    (cond
		     ((eq jabber-form-type 'register)
		      `(query ((xmlns . "jabber:iq:search"))
			      ,@(jabber-parse-register-form)))
		     ((eq jabber-form-type 'xdata)
		      `(query ((xmlns . "jabber:iq:search"))
			      ,(jabber-parse-xdata-form)))
		     (t
		      (error "Unknown form type: %s" jabber-form-type)))
		    #'jabber-process-data #'jabber-process-search-result
		    #'jabber-report-success text))

  (message "Search sent"))

(defun jabber-remove-register (&rest ignore)
  "Cancel registration.  See `jabber-process-register-or-search'."

  (if (yes-or-no-p (concat "Are you sure that you want to cancel your registration to " jabber-submit-to "? "))
      (jabber-send-iq jabber-submit-to
		      "set"
		      '(query ((xmlns . "jabber:iq:register"))
			      (remove))
		      #'jabber-report-success "Unregistration"
		      #'jabber-report-success "Unregistration")))

(add-to-list 'jabber-iq-get-xmlns-alist 'jabber-return-version)
(defun jabber-return-version (xml-data)
  "Return client version as defined in JEP-0092.  Sender and ID are
determined from the incoming packet passed in XML-DATA."
  ;; Things we might check: does this iq message really have type='get' and
  ;; exactly one child, namely query with xmlns='jabber:iq:version'?
  ;; Then again, jabber-process-iq should take care of that.
  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id)))
    (jabber-send-iq to "result"
		    `(query ((xmlns . "jabber:iq:version"))
			    (name () "jabber.el")
			    (version () "0.4")
			    ;; Booting... /vmemacs.el
			    ;; Shamelessly stolen from someone's sig.
			    (os () ,(jabber-escape-xml (emacs-version))))
		    nil nil nil nil
		    id)))



(defun jabber-do-logon (xml-data closure-data)
  "send username and password in logon attempt"
  (cond
   ((string= (jabber-xml-get-attribute xml-data 'type) "result")
    (let (auth)
      (if (jabber-xml-get-children (jabber-iq-query xml-data) 'digest)
	  ;; SHA1 digest passwords allowed
	  (let ((passwd (jabber-read-passwd)))
	    (if passwd
		(setq auth `(digest () ,(sha1 (concat jabber-session-id passwd))))))
	(if (yes-or-no-p "Jabber server only allows cleartext password transmission!  Continue? ")
	    (let ((passwd (jabber-read-passwd)))
	      (if passwd
		  (setq auth `(password () ,passwd))))))
      
      ;; If auth is still nil, user cancelled process somewhere
      (if auth
	  (jabber-send-iq jabber-server
			  "set"
			  `(query ((xmlns . "jabber:iq:auth"))
				  (username () ,jabber-username)
				  ,auth
				  (resource () ,jabber-resource))
			  #'jabber-process-logon t
			  #'jabber-process-logon nil)
	(jabber-disconnect))))
   (t
    (error "Logon error ended up in the wrong place"))))
	
(defun jabber-process-logon (xml-data closure-data)
  "receive login success or failure, and request roster.
CLOSURE-DATA should be t on success and nil on failure."
  (if closure-data
      ;; Logon success
      (progn
	(jabber-send-iq jabber-server
			"get" 
			'(query ((xmlns . "jabber:iq:roster")))
			#'jabber-process-roster 'initial
			#'jabber-report-success "Roster retrieval")

	;; You are by no means forced to send presence when connected.
	;;(jabber-send-sexp '((presence)))
	)

    ;; Logon failure
    (jabber-report-success xml-data "Logon")
    (jabber-disconnect)))

(defun jabber-process-register-secondtime (xml-data closure-data)
  "Receive registration success or failure.
CLOSURE-DATA is either 'success or 'error."
  (setq jabber-register-p nil)
  (cond
   ((eq closure-data 'success)
    (message "Registration successful.  Your JID is %s@%s."
	     jabber-username jabber-server)
    (sit-for 3)
    (jabber-get-auth jabber-server))
   (t
    (jabber-report-success xml-data "Account registration")
    (sit-for 3)
    (jabber-disconnect))))





   









(defun jabber-send-groupchat (group body)
  "send a message to a groupchat"
  (jabber-send-message group body nil "groupchat"))

(defun jabber-send-chat (to body)
  "send a chat message to someone"
  (jabber-send-message to body nil "chat"))



(defun jabber-send-message (to body subject type)
  "send a message tag to the server"
  (interactive (list (jabber-read-jid-completing "to: ")
		     (jabber-read-with-input-method "body: ")
		     (jabber-read-with-input-method "subject: ")
		     (read-string "type: ")))
  (jabber-send-sexp `(message ((to . ,to)
                               ,(if (> (length type) 0)
                                    `(type . ,type)))
                              ,(if (> (length subject) 0)
                                   `(subject () ,(jabber-escape-xml subject)))
                              ,(if (> (length body) 0)
                                   `(body () ,(jabber-escape-xml body))))))

(defun jabber-get-version (to)
  "Request software version"
  (interactive (list (jabber-read-jid-completing "Request version of: ")))
  ;; XXX: you will not get any result unless you add the resource to the JID.
  (jabber-send-iq to
		  "get"
		  '(query ((xmlns . "jabber:iq:version")))
		  #'jabber-process-data #'jabber-process-version
		  #'jabber-process-data "Version request failed"))

(defun jabber-roster-change (jid name groups)
  "Add or change a roster item."
  (interactive (let* ((jid (intern (jabber-read-jid-completing "Add/change JID: ") jabber-jid-obarray))
		      (name (get jid 'name))
		      (groups (get jid 'groups)))
		 (list jid (jabber-read-with-input-method (format "Name: (default `%s') " name) nil nil name)
		       (read-from-minibuffer (format "Groups: (default `%S') " groups) nil nil t nil (format "%S" groups) t))))
  ;; If new fields are added to the roster XML structure in a future standard,
  ;; they will be clobbered by this function.
  (jabber-send-iq nil "set" 
		  (list 'query (list (cons 'xmlns "jabber:iq:roster"))
			(list 'item (append
				     (list (cons 'jid (symbol-name jid)))
				     (if (and name (> (length name) 0))
					 (list (cons 'name name))))
			      (mapcar (lambda (x) `(group () ,x))
				      groups))) 
		  #'jabber-report-success "Roster item change"
		  #'jabber-report-success "Roster item change"))

(defun jabber-roster-delete (jid)
  (interactive (list (jabber-read-jid-completing "Delete from roster: ")))
  (jabber-send-iq nil "set"
		  `(query ((xmlns . "jabber:iq:roster"))
			  (item ((jid . ,jid)
				 (subscription . "remove"))))
		  #'jabber-report-success "Roster item removal"
		  #'jabber-report-success "Roster item removal"))

(defun jabber-groupchat-leave (group)
  "leave a groupchat"
  (interactive (list (completing-read "group: "
                                      (mapcar (lambda (item) (cons item nil))
                                              *jabber-active-groupchats*))))
  (let ((lst (member group *jabber-active-groupchats*)))
    (setcar lst (cadr lst))
    (setcdr lst (cddr lst)))
  ;; This isn't right.  Unavailable presence should be sent to
  ;; group@server/nick, not group@server.
  (jabber-send-sexp `(presence ((to . ,group)
                                (type . "unavailable")))))

(defun jabber-groupchat-join (group nickname)
  "join a groupchat"
  (interactive (list (jabber-read-jid-completing "group: ")
		     (jabber-read-with-input-method (format "Nickname: (default %s) "
							    jabber-nickname) 
						    nil nil jabber-nickname)))
  (jabber-send-sexp `(presence ((to . ,(format "%s/%s" group nickname)))))

  (if (not (member group *jabber-active-groupchats*))
      (setq *jabber-active-groupchats* (cons group *jabber-active-groupchats*)))
  (jabber-groupchat-display group))

(defun jabber-groupchat-get-config (group)
  "Ask for MUC configuration form"
  (interactive (list (jabber-read-jid-completing "group: ")))
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





(defun jabber-get-auth (to)
  "Send IQ get request in namespace \"jabber:iq:auth\"."
  (jabber-send-iq to
		  "get"
		  `(query ((xmlns . "jabber:iq:auth"))
			  (username () ,jabber-username))
		  #'jabber-do-logon nil
		  #'jabber-report-success "Impossible error - auth field request"))

(defun jabber-get-register (to)
  "Send IQ get request in namespace \"jabber:iq:register\"."
  (interactive (list (jabber-read-jid-completing "Register with: ")))
  (jabber-send-iq to
		  "get"
		  '(query ((xmlns . "jabber:iq:register")))
		  #'jabber-process-data #'jabber-process-register-or-search
		  #'jabber-report-success "Registration"))

(defun jabber-get-search (to)
  "Send IQ get request in namespace \"jabber:iq:search\"."
  (interactive (list (jabber-read-jid-completing "Search what database: ")))
  (jabber-send-iq to
		  "get"
		  '(query ((xmlns . "jabber:iq:search")))
		  #'jabber-process-data #'jabber-process-register-or-search
		  #'jabber-report-success "Search field retrieval"))

(defun jabber-get-browse (to)
  "send a browse infoquery request to someone"
  (interactive (list (jabber-read-jid-completing "browse: ")))
  (jabber-send-iq to 
                  "get"
                  '(query ((xmlns . "jabber:iq:browse")))
                  #'jabber-process-data #'jabber-process-browse
		  #'jabber-process-data "Browse failed"))




(provide 'jabber)
