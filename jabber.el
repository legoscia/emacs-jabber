;; jabber.el - a minimal jabber client

;; Copyright (C) 2002 - tom berger - object@intelectronica.net
;; Copyright (C) 2003 - Magnus Henoch - mange@freemail.hu

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

(defvar *jabber-connection* nil
  "the process that does the actual connection")

(defvar *jabber-roster* nil
  "the roster list")

(defvar jabber-jid-obarray (make-vector 127 0)
  "obarray for keeping JIDs")

(defvar *jabber-active-groupchats* nil
  "a list of the groupchats we are currently in")

(defvar *jabber-open-info-queries* nil
  "an alist of open query id and their callback functions")

(defvar *jabber-connected* nil
  "boolean - are we connected")

(defvar *jabber-current-status* "na"
  "the users current presence staus")

(defvar *jabber-current-show* ""
  "the users current presence show")

(defvar *jabber-current-priority* 0
  "the user's current priority")

(defvar *jabber-status-history* nil
  "history of status messages")

(defvar jabber-jid-history nil
  "History of entered JIDs")

(defvar *jabber-sound-playing* nil
  "is a sound playing right now?")

(defvar *xmlq* ""
  "a string with all the incoming xml that is waiting to be parsed")

(defvar jabber-register-p nil
  "Register a new account in this session?")

(defvar jabber-session-id nil
  "id of the current session")

(defvar jabber-chatting-with nil
  "JID of the person you are chatting with")

(defvar jabber-group nil
  "the groupchat you are participating in")

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

(defface jabber-roster-user-offline
  '((t (:foreground "grey" :weight light :slant italic)))
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

(defcustom jabber-server "magaf.org" 
  "jabber server" 
  :type 'string
  :group 'jabber)

(defcustom jabber-username "emacs"
  "jabber username" 
  :type 'string
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

(defcustom jabber-port 5222
  "jabber port" 
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

(defgroup jabber-alerts nil "auditory and visual alerts for jabber events"
  :group 'jabber)

(defcustom jabber-alert-message-hooks '(jabber-message-beep jabber-message-echo)
  "Hooks run when a new message arrives.

Arguments are FROM, BUFFER, TEXT and PROPOSED-ALERT.  FROM is the JID
of the sender, BUFFER is the the buffer where the message can be read,
and TEXT is the text of the message.  PROPOSED-ALERT is the string
returned by `jabber-alert-message-function' for these arguments, so that
hooks do not have to call it themselves."
  :type 'hook
  :options '(jabber-message-beep jabber-message-wave jabber-message-echo jabber-message-switch jabber-message-ratpoison jabber-message-screen)
  :group 'jabber-alerts)

(defcustom jabber-alert-message-function
  'jabber-message-default-message
  "Function for constructing message alert messages.

Arguments are FROM, BUFFER, and TEXT.  This function should return a
string containing an appropriate text message, or nil if no message
should be displayed.

The provided hooks displaying a text message get it from this function,
and show no message if it returns nil.  Other hooks do what they do
every time."
  :type 'function
  :group 'jabber-alerts)

(defcustom jabber-alert-presence-hooks '(jabber-presence-beep jabber-presence-update-roster jabber-presence-echo)
  "Hooks run when a user's presence changes.

Arguments are WHO, OLDSTATUS, NEWSTATUS, STATUSTEXT and
PROPOSED-ALERT.  WHO is a symbol whose text is the JID of the contact,
and which has various interesting properties.  OLDSTATUS is the old
presence or nil if disconnected.  NEWSTATUS is the new presence, or
one of \"subscribe\", \"unsubscribe\", \"subscribed\" and
\"unsubscribed\".  PROPOSED-ALERT is the string returned by
`jabber-alert-presence-message-function' for these arguments."
  :type 'hook
  :options '(jabber-presence-beep jabber-presence-wave jabber-presence-update-roster jabber-presence-switch jabber-presence-ratpoison jabber-presence-screen jabber-presence-echo)
  :group 'jabber-alerts)

(defcustom jabber-alert-presence-message-function
  'jabber-presence-default-message
  "Function for constructing presence alert messages.

Arguments are WHO, OLDSTATUS, NEWSTATUS and STATUSTEXT.  See
`jabber-alert-presence-hooks' for documentation. This function
should return a string containing an appropriate text message, or nil
if no message should be displayed.

The provided hooks displaying a text message get it from this function.
All hooks refrain from action if this function returns nil."
  :type 'function
  :group 'jabber-alerts)

(defcustom jabber-alert-info-message-hooks '(jabber-info-beep jabber-info-echo)
  "Hooks run when an info request is completed.

First argument is WHAT, a symbol telling the kind of info request completed.
That might be 'roster, for requested roster updates, and 'browse, for
browse requests.  Second argument in BUFFER, a buffer containing the result.
Third argument is PROPOSED-ALERT, containing the string returned by
`jabber-alert-info-message-function' for these arguments."
  :type 'hook
  :options '(jabber-info-beep jabber-info-wave jabber-info-ratpoison jabber-info-screen jabber-info-echo jabber-info-switch)
  :group 'jabber-alerts)

(defcustom jabber-alert-info-message-function
  'jabber-info-default-message
  "Function for constructing info alert messages.

Arguments are WHAT, a symbol telling the kind of info request completed,
and BUFFER, a buffer containing the result."
  :type 'function
  :group 'jabber-alerts)

(defcustom jabber-info-message-alist
  '((roster . "Roster display updated")
    (browse . "Browse request completed"))
  "Alist for info alert messages, used by `jabber-info-default-message'."
  :type '(alist :key-type symbol :value-type string
		:options (roster browse))
  :group 'jabber-alerts)

(defcustom jabber-alert-message-wave nil
  "a sound file to play when a message arrived"
  :type 'file
  :group 'jabber-alerts)

(defcustom jabber-alert-presence-wave nil
  "a sound file to play when a presence arrived"
  :type 'file
  :group 'jabber-alerts)

(defcustom jabber-alert-info-wave nil
  "a sound file to play when an info query result arrived"
  :type 'file
  :group 'jabber-alerts)

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
   (nil . jabber-roster-user-offline))
 "Mapping from presence types to faces")

(defconst jabber-presence-strings
  '(("" . "Online")
    ("away" . "Away")
    ("xa" . "Extended Away")
    ("dnd" . "Do not Disturb")
    ("chat" . "Chatty")
    (nil . "Offline"))
  "Mapping from presence types to readable strings")

(defmacro jabber-define-status-key (title show)
  (list 'let (list ( list 'func (list 'make-symbol (list 'concat "jabber-send-presence-" show)))
         (list 'menu-item (list 'make-symbol (list 'concat "jabber-menu-status-" show))))
     (list 'fset 'func `(lambda () (interactive)
                           (jabber-send-presence ,show (read-string "status: ") *jabber-current-priority*)))
     (list 'define-key 'global-map
           (list 'vector ''menu-bar ''jabber-menu ''jabber-menu-status 'menu-item)
           (list 'cons title 'func))))

(dolist (presence jabber-presence-strings)
  (jabber-define-status-key (cdr presence) (car presence)))
;;;(jabber-define-status-key "Online" "")
;;;(jabber-define-status-key "Away" "away")
;;;(jabber-define-status-key "Extended Away" "xa")
;;;(jabber-define-status-key "Do not Disturb" "dnd")
;;;(jabber-define-status-key "Unavailable" "na")

(defconst jabber-iq-get-xmlns-alist
  (list
   (cons "jabber:iq:version" 'jabber-return-version)
   (cons "http://jabber.org/protocol/disco#info" 'jabber-return-disco-info))
  "Mapping from XML namespace to handler for IQ GET requests.")

(defconst jabber-iq-set-xmlns-alist
  (list
   (cons "jabber:iq:roster" (lambda (x) (jabber-process-roster x nil))))
  "Mapping from XML namespace to handler for IQ SET requests.")

(defconst jabber-advertised-features
  (list "jabber:iq:version"
	"http://jabber.org/protocol/disco#info")
  "Features advertised on service discovery requests")

(defconst jabber-jid-menu
  (list
   (cons "Start chat" 'jabber-chat-with)
   (cons "Send message" 'jabber-send-message)
   (cons "Send items disco query" 'jabber-get-disco-items)
   (cons "Send info disco query" 'jabber-get-disco-info)
   (cons "Send browse query" 'jabber-get-browse)
   (cons "Request software version" 'jabber-get-version)
   (cons "Send subscription request" 'jabber-send-subscription-request)
   (cons "Add/modify roster entry" 'jabber-roster-change)
   (cons "Delete roster entry" 'jabber-roster-delete))
  "Menu items for JID menu") 

(defconst jabber-error-messages
  (list
   (cons 'bad-request "Bad request")
   (cons 'conflict "Conflict")
   (cons 'feature-not-implemented "Feature not implemented")
   (cons 'forbidden "Forbidden")
   (cons 'gone "Gone")
   (cons 'internal-server-error "Internal server error")
   (cons 'item-not-found "Item not found")
   (cons 'jid-malformed "JID malformed")
   (cons 'not-acceptable "Not acceptable")
   (cons 'not-allowed "Not allowed")
   (cons 'payment-required "Payment required")
   (cons 'recipient-unavailable "Recipient unavailable")
   (cons 'redirect "Redirect")
   (cons 'registration-required "Registration required")
   (cons 'remote-server-not-found "Remote server not found")
   (cons 'remote-server-timeout "Remote server timeout")
   (cons 'resource-constraint "Resource constraint")
   (cons 'service-unavailable "Service unavailable")
   (cons 'subscription-required "Subscription required")
   (cons 'undefined-condition "Undefined condition")
   (cons 'unexpected-request "Unexpected request"))
  "String descriptions of XMPP stanza errors")

(defconst jabber-legacy-error-messages
  (list
   (cons 302 "Redirect")
   (cons 400 "Bad request")
   (cons 401 "Unauthorized")
   (cons 402 "Payment required")
   (cons 403 "Forbidden")
   (cons 404 "Not found")
   (cons 405 "Not allowed")
   (cons 406 "Not acceptable")
   (cons 407 "Registration required")
   (cons 408 "Request timeout")
   (cons 409 "Conflict")
   (cons 500 "Internal server error")
   (cons 501 "Not implemented")
   (cons 502 "Remote server error")
   (cons 503 "Service unavailable")
   (cons 504 "Remote server timeout")
   (cons 510 "Disconnected"))
  "String descriptions of legacy errors (JEP-0086)")
  
(defun jabber-report-success (xml-data context)
  "IQ callback reporting success or failure of the operation.
CONTEXT is a string describing the action."
  (let ((type (jabber-xml-get-attribute xml-data 'type)))
    (message (concat context
		     (if (string= type "result")
			 " succeeded"
		       (concat
			" failed: "
			(jabber-parse-error (jabber-iq-error xml-data))))))))

(defun jabber-parse-error (error-xml)
  "Parse the given <error/> tag and return a string fit for human consumption.
See secton 9.3, Stanza Errors, of XMPP Core, and JEP-0086, Legacy Errors."
  (let ((error-type (jabber-xml-get-attribute error-xml 'type))
	(error-code (jabber-xml-get-attribute error-xml 'code))
	condition text)
    (if error-type
	;; If the <error/> tag has a type element, it is new-school.
	(dolist (child (xml-node-children error-xml))
	  (when (string=
		 (jabber-xml-get-attribute child 'xmlns)
		 "urn:ietf:params:xml:ns:xmpp-stanzas")
	    (if (eq (xml-node-name child) 'text)
		(setq text (car (xml-node-children child)))
	      (setq condition
		    (or (cdr (assq (xml-node-name child) jabber-error-messages))
			(symbol-name (xml-node-name child)))))))
      (setq condition (or (cdr (assq (string-to-number error-code) jabber-legacy-error-messages))
			  error-code))
      (setq text (car (xml-node-children error-xml))))
    (concat condition
	    (if text (format ": %s" text)))))

(defun jabber-customize ()
  "customize jabber options"
  (interactive)
  (customize-group 'jabber))

(defun jabber-escape-xml (str)
  "escape strings for xml"
  (if (stringp str)
      (let ((newstr str))
	(setq newstr (replace-regexp-in-string "&" "&amp;" newstr t t))
	(setq newstr (replace-regexp-in-string "<" "&lt;" newstr t t))
	(setq newstr (replace-regexp-in-string ">" "&gt;" newstr t t))
	(setq newstr (replace-regexp-in-string "'" "&apos;" newstr t t))
	(setq newstr (replace-regexp-in-string "\"" "&quot;" newstr t t))
	newstr)
    str))

(defun jabber-unescape-xml (str)
  "unescape xml strings"
  ;; Eventually this can be done with `xml-substitute-special', but the
  ;; version in xml.el of GNU Emacs 21.3 is buggy.
  (if (stringp str)
      (let ((newstr str))
	(setq newstr (replace-regexp-in-string "&quot;" "\"" newstr t t))
	(setq newstr (replace-regexp-in-string "&apos;" "'" newstr t t))
	(setq newstr (replace-regexp-in-string "&gt;" ">" newstr t t))
	(setq newstr (replace-regexp-in-string "&lt;" "<" newstr t t))
	(setq newstr (replace-regexp-in-string "&amp;" "&" newstr t t))
	newstr)
    str))

(defun jabber-play-sound-file (soundfile)
  (if (not *jabber-sound-playing*)
      (progn
	(setq *jabber-sound-playing* t)
	(run-with-idle-timer 0.01 nil 
			     (lambda (sf)
			       (condition-case nil
				   ;; play-sound-file might display "Could not set sample rate" in
				   ;; echo area.  Don't let this erase the previous message.
				   (let ((old-message (current-message)))
				     (play-sound-file sf)
				     (setq *jabber-sound-playing* nil)
				     (message old-message))
				 (error (setq *jabber-sound-playing* nil))))
			     soundfile))))

(defun sexp2xml (sexp)
  "converts an SEXP in the format (tagname ((attribute-name . attribute-value)...) children...) and converts it to well-formatted xml."
  (cond
   ((stringp sexp)
    sexp)
   ((listp (car sexp))
    (let ((xml ""))
      (dolist (tag sexp)
	(setq xml (concat xml (sexp2xml tag))))
      xml))
   (t
    (let ((xml ""))
      (setq xml (concat "<" 
			(symbol-name (car sexp))))
      (dolist (attr (cadr sexp))
	(if (consp attr)
	    (setq xml (concat xml
			      (format " %s='%s'"
				      (symbol-name (car attr))
				      (cdr attr))))))
      (if (cddr sexp)
	  (progn
	    (setq xml (concat xml ">"))
	    (dolist (child (cddr sexp))
	      (setq xml (concat xml
				(sexp2xml child))))
	    (setq xml (concat xml
			      "</"
			      (symbol-name (car sexp))
			      ">")))
	(setq xml (concat xml
			  "/>")))
      xml))))

(defmacro jabber-xml-get-attribute (node attribute)
    "Get from NODE the value of ATTRIBUTE.
Return nil if the attribute was not found. If `xml-get-attribute-or-nil'
is not present, emulate it with `xml-get-attribute'."
    (if (fboundp 'xml-get-attribute-or-nil)
	`(xml-get-attribute-or-nil ,node ,attribute)
      `(let ((result (xml-get-attribute ,node ,attribute)))
	 (and (> (length result) 0) result))))
    
(defun jabber-send-sexp (sexp)
  "send the xml corresponding to SEXP to the jabber server"
  (if jabber-debug
      (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
	(goto-char (point-max))
	(insert (format "sending %S\n\n" sexp))))
  (process-send-string *jabber-connection* (sexp2xml sexp)))

;;; XXX: include nicknames
(defun jabber-read-jid-completing (prompt)
  "read a jid out of the current roster from the minibuffer."
  (let ((jid-at-point (get-text-property (point) 'jabber-jid)))
    (completing-read (concat prompt
			     (if jid-at-point
				 (format "(default %s) " jid-at-point)))
		     (mapcar (lambda (item) (cons (symbol-name item) nil))
			     *jabber-roster*)
		     nil nil nil 'jabber-jid-history jid-at-point)))

(defun jabber-read-node (prompt)
  "Read node name, taking default from disco item at point."
  (let ((node-at-point (get-text-property (point) 'jabber-node)))
    (read-string (concat prompt
			 (if node-at-point
			     (format "(default %s) " node-at-point)))
		 node-at-point)))

(defvar jabber-roster-mode-map nil)

(defun jabber-roster-mode ()
  "\\{jabber-roster-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'jabber-roster-mode
	mode-name "jabber-roster")
  (use-local-map jabber-roster-mode-map)
  (setq buffer-read-only t))

(put 'jabber-roster-mode 'mode-class 'special)

(unless jabber-roster-mode-map
  (setq jabber-roster-mode-map (make-sparse-keymap))
  (define-key jabber-roster-mode-map "\C-c\C-c" 'jabber-popup-menu)
  (define-key jabber-roster-mode-map [mouse-2] 'jabber-popup-menu)
  )

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
  (setq jabber-browse-mode-map (make-sparse-keymap))
  (define-key jabber-browse-mode-map "\C-c\C-c" 'jabber-popup-menu)
  (define-key jabber-roster-mode-map [mouse-2] 'jabber-popup-menu)
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
    (fset send-fun `(lambda (body) (interactive (list (read-string "" ,(char-to-string key))))
		      (jabber-send-groupchat jabber-group body)
		      (setq buffer-read-only nil)
		      (goto-char (point-max))
		      (if (not (eq major-mode 'jabber-groupchat-mode))
			  (jabber-groupchat-mode))))
    (define-key jabber-groupchat-mode-map (char-to-string key) send-fun)))

(defun jabber-groupchat-display (group &optional nick body)
  "display the groupchat window and an incoming message, if there is one"
  (with-current-buffer (get-buffer-create (concat "*-jabber-groupchat-:-" group "-*"))
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (if body (insert (propertize (concat "[" (substring (current-time-string) 11 16) "] " nick)
                                 'face 'jabber-chat-prompt-foreign)
                     "> " body "\n"))
    (if (not (eq major-mode 'jabber-groupchat-mode))
	(jabber-groupchat-mode))
    (setq jabber-group group)
    (run-hook-with-args 'jabber-alert-message-hooks group (current-buffer) body (funcall jabber-alert-message-function group (current-buffer) body))))

(defun jabber-chat-mode ()
  "\\{jabber-chat-mode-map}"
  (kill-all-local-variables)
  (make-local-variable 'jabber-chatting-with)
  (setq major-mode 'jabber-chat-mode
        mode-name "jabber-chat")
  (use-local-map jabber-chat-mode-map))

(put 'jabber-chat-mode 'mode-class 'special)

(defvar jabber-chat-mode-map (make-keymap))

(suppress-keymap jabber-chat-mode-map)


(dolist (key (append "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890`~!@#$%^&*()_+-=[]{}|';:/?.,>< " nil))
  (let ((send-fun (make-symbol (concat "jabber-chat-buffer-send-" (char-to-string key)))))
    (fset send-fun `(lambda (body) (interactive (list (read-string "" ,(char-to-string key) nil nil t)))
		      (jabber-send-chat jabber-chatting-with body)
		      (setq buffer-read-only nil)
		      (goto-char (point-max))
		      (insert (propertize (concat "[" (substring (current-time-string) 11 16) "] " jabber-username)
                                          'face 'jabber-chat-prompt-local) "> " body "\n")
		      (setq buffer-read-only t)))
    (define-key jabber-chat-mode-map (char-to-string key) send-fun)))

(defun jabber-chat-display (&optional from body)
  "display the chat window and a new message, if there is one"
  (with-current-buffer (get-buffer-create (concat "*-jabber-chat-:-" (jabber-jid-displayname from) "-*"))
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (if body (insert (propertize (concat "[" (substring (current-time-string) 11 16) "] " (jabber-jid-displayname from))
				 'face 'jabber-chat-prompt-foreign)
		     "> " body "\n"))
    (setq buffer-read-only t)

    ;; Setting the major mode more than once will wipe out buffer-local
    ;; variables, therefore caution.
    (if (not (eq major-mode 'jabber-chat-mode))
	(jabber-chat-mode))
    ;; XXX: not resource-safe
    (if from
	(setq jabber-chatting-with (jabber-jid-user from)))
    (run-hook-with-args 'jabber-alert-message-hooks from (current-buffer) body (funcall jabber-alert-message-function from (current-buffer) body))))

(defun jabber-chat-with (jid)
  "open an empty chat window for chatting with JID"
  (interactive (list (jabber-read-jid-completing "chat with:")))
  (jabber-chat-display (concat jid "/chat") nil))

(defun jabber-jid-username (string)
  "return the username portion of a JID"
  (string-match "\\(.*\\)@.*\\(/.*\\)?" string)
  (match-string 1 string))

(defun jabber-jid-user (string)
  "return the user (username@server) portion of a JIF"
  ;;transports don't have @, so don't require it
  ;;(string-match ".*@[^/]*" string)
  (string-match "[^/]*" string)
  (match-string 0 string))

(defun jabber-jid-displayname (string)
  "return the name of the user, if given in roster, else username@server"
  (let ((user (intern (jabber-jid-user string) jabber-jid-obarray)))
    (let ((roster-item (car (memq user *jabber-roster*))))
      (if (and roster-item
	       (> (length (get roster-item 'name)) 0))
	  (get roster-item 'name)
	(symbol-name user)))))

(defun jabber-jid-resource (string)
  "return the resource portion of a JID"
  (string-match "\\(.*@.*\\)/\\(.*\\)" string)
  (match-string 2 string))

(defun jabber-iq-query (xml-data)
  "Return the query part of an IQ stanza.
An IQ stanza may have zero or one query child, and zero or one <error/> child.
The query child is often but not always <query/>."
  (let (query)
    (dolist (x (xml-node-children xml-data))
      (if (and
	   (listp x)
	   (not (eq (xml-node-name x) 'error)))
	  (setq query x)))
    query))

(defun jabber-iq-error (xml-data)
  "Return the <error/> part of an IQ stanza, if any."
  (and (listp (car (xml-node-children xml-data)))
       (car (xml-get-children xml-data 'error))))

(defun jabber-iq-xmlns (xml-data)
  "Return the namespace of an IQ stanza, i.e. the namespace of its query part."
  (jabber-xml-get-attribute (jabber-iq-query xml-data) 'xmlns))

(defun jabber-process-message (from subject body thread type)
  "process incoming messages"
  (cond
   ((string= type "groupchat")
    (jabber-groupchat-display (jabber-jid-user from) 
                              (jabber-jid-resource from)
                              (jabber-unescape-xml body))
    )
   (t
    (jabber-chat-display from 
                         (jabber-unescape-xml body)))))


(defun jabber-process-subscription-request (from presence-status)
  "process an incoming subscription request"
  (run-hook-with-args 'jabber-alert-presence-hooks (intern from jabber-jid-obarray) nil "subscribe" presence-status (funcall jabber-alert-presence-message-function (intern from jabber-jid-obarray) nil "subscribe" presence-status))
  (jabber-send-sexp 
   (list 'presence (list (cons 'to from)
			 (cons 'type (if (yes-or-no-p (format "the user  - %s -  has requested to subscribe to your presence (%s). allow? "
							   from
							   (jabber-unescape-xml presence-status)))
					 "subscribed"
				       "unsubscscribed")))))
  (when (yes-or-no-p (format "Do you want to subscribe to %s's presence? " from))
    (jabber-send-sexp
     (list 'presence (list (cons 'to from)
			   (cons 'type "subscribe"))))))


(defun jabber-process-presence (from to presence-show presence-status type priority)
  "process incoming presence tags"
  (cond
   ((string= type "subscribe")
    (run-with-idle-timer 0.01 nil #'jabber-process-subscription-request from presence-status))
   
   (t
    (dolist (buddy *jabber-roster*)
      (if (string= (symbol-name buddy) (jabber-jid-user from))
	  (let* ((oldstatus (get buddy 'show))
		 (resource (or (jabber-jid-resource from) ""))
		 (resource-plist (cdr (assoc resource
					     (get buddy 'resources))))
		 newstatus)
	    (cond
	     ((or
	       (string= type "unavailable")
	       (string= type "error"))
	      (setq resource-plist
		    (plist-put resource-plist 'connected nil))
	      (setq resource-plist
		    (plist-put resource-plist 'show nil))
	      (setq resource-plist
		    (plist-put resource-plist 'status (jabber-unescape-xml presence-status)))
	      )
	     ((or
	       (string= type "unsubscribe")
	       (string= type "subscribed")
	       (string= type "unsubscribed"))
	      ;; Do nothing, except letting the user know.  The Jabber protocol
	      ;; places all this complexity on the server.
	      (setq newstatus type))
	     (t
	      (setq resource-plist
		    (plist-put resource-plist 'connected t))
	      (setq resource-plist
		    (plist-put resource-plist 'show (or presence-show "")))
	      (setq resource-plist
		    (plist-put resource-plist 'status (jabber-unescape-xml presence-status)))
	      (setq resource-plist
		    (plist-put resource-plist 'priority priority))
	      (setq newstatus (or presence-show ""))))

	    ;; this is for `assoc-set!' in guile
	    (if (assoc resource (get buddy 'resources))
		(setcdr (assoc resource (get buddy 'resources)) resource-plist)
	      (put buddy 'resources (cons (cons resource resource-plist) (get buddy 'resources))))
	    (jabber-prioritize-resources buddy)

	    (run-hook-with-args 'jabber-alert-presence-hooks buddy oldstatus newstatus (jabber-unescape-xml presence-status) 
				(funcall jabber-alert-presence-message-function buddy oldstatus newstatus (jabber-unescape-xml presence-status)))))))))

(defun jabber-prioritize-resources (buddy)
  "Set connected, show and status properties for BUDDY from highest-priority resource."
  (let ((resource-alist (get buddy 'resources))
	(highest-priority nil))
    ;; Reset to nil at first, for cases (a) resource-alist is nil
    ;; and (b) all resources are disconnected.
    (put buddy 'connected nil)
    (put buddy 'show nil)
    (put buddy 'status nil)
    (mapc (lambda (resource)
	    (when (plist-get (cdr resource) 'connected)
		(let* ((resource-plist (cdr resource))
		       (priority (plist-get resource-plist 'priority)))
		  (when (or (null highest-priority)
			    (and priority
				 (> priority highest-priority)))
		    ;; if no priority specified, interpret as zero
		    (setq highest-priority (or priority 0))
		    (put buddy 'connected (plist-get resource-plist 'connected))
		    (put buddy 'show (plist-get resource-plist 'show))
		    (put buddy 'status (plist-get resource-plist 'status))))))
	  resource-alist)))

(defun jabber-popup-menu ()
  "Popup menu of things commonly done to JIDs"
  (interactive)
  (call-interactively (widget-choose "Actions" jabber-jid-menu last-command-event)))

(defun jabber-sort-roster ()
  "sort roster according to online status"
  (setq *jabber-roster*
	(sort *jabber-roster*
	      (lambda (a b)
		(let ((a-show (and (get a 'connected) (get a 'show)))
		      (b-show (and (get b 'connected) (get b 'show))))
		  (> (length (member a-show jabber-sort-order))
		     (length (member b-show jabber-sort-order))))))))

(defun jabber-display-roster ()
  "switch to the main jabber buffer and refresh the roster display to reflect the current information"
  (interactive)
  (with-current-buffer (process-buffer *jabber-connection*)
    (if (not (eq major-mode 'jabber-roster-mode))
	(jabber-roster-mode))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (propertize jabber-server 'face 'jabber-title-large) "\n__________________________________\n\n")
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-2] #'jabber-send-presence)
      (insert (propertize (format " - %s (%s) -"
				  (cdr (assoc *jabber-current-show* jabber-presence-strings))
                                  *jabber-current-status*)
                          'face (or (cdr (assoc *jabber-current-show* jabber-presence-faces))
				    'jabber-roster-user-online)
                          'mouse-face (cons 'background-color "light grey")
                          'keymap map)
              "\n__________________________________\n\n"))

    (jabber-sort-roster)
    (dolist (buddy *jabber-roster*)
      (let ((buddy-str (concat (if (get buddy 'connected)
				   " * "
				 "   ")
			       (if (> (length (get buddy 'name)) 0)
				   (get buddy 'name)
				 (symbol-name buddy))
			       (format " - %s" (or
						(cdr (assoc (get buddy 'show) jabber-presence-strings))
						(get buddy 'show)))
			       (if (get buddy 'status)
				   (format " (%s)" (get buddy 'status)))
			       (if jabber-debug
				   (format " --- [%S] ---" (symbol-plist buddy)))
			       )))
	(add-text-properties 0
			   (length buddy-str)
			   (list
			    'face
			    (if (get buddy 'connected)
				(or (cdr (assoc (get buddy 'show) jabber-presence-faces))
				    'jabber-roster-user-online)
			      'jabber-roster-user-offline)
			    'mouse-face
			    (append '(:background-color "light grey") (get-text-property 0 'face buddy-str))
			    'help-echo
			    (symbol-name buddy)
			    'jabber-jid
			    (symbol-name buddy))
			   buddy-str)
	;; (let ((map (make-sparse-keymap))
;; 	      (chat-with-func (make-symbol (concat "jabber-chat-with" (symbol-name buddy)))))
;; 	  (fset chat-with-func `(lambda () (interactive) (jabber-chat-with ,(symbol-name buddy))))
;; 	  (define-key map [mouse-2] chat-with-func)
;; 	  (put-text-property 0
;; 			     (length buddy-str)
;; 			     'keymap 
;; 			     map
;; 			     buddy-str))
	(insert buddy-str "\n\n")))
    (insert "__________________________________")
    (goto-char (point-min))
    (setq buffer-read-only t)
    (if (interactive-p)
	(run-hook-with-args 'jabber-alert-info-message-hooks 'roster (current-buffer) (funcall jabber-alert-info-message-function 'roster (current-buffer))))))

(defun jabber-process-roster (xml-data closure-data)
  "process an incoming roster infoquery result
CLOSURE-DATA should be 'initial if initial roster push, nil otherwise."

  ;; Perform sanity check on "from" attribute: it should be either absent
  ;; or match our own JID.
  (let ((from (jabber-xml-get-attribute xml-data 'from))
	(type (jabber-xml-get-attribute xml-data 'type))
	(id (jabber-xml-get-attribute xml-data 'id)))
    (if (not (or (null from)
		 (string= (jabber-jid-user from) (concat jabber-username "@" jabber-server))))
	(message "Roster push with invalid \"from\": \"%s\"" from)

      ;; If *jabber-roster* is empty, we just fill up the roster with the given data.
      ;; If not, we have received a partial roster update, so just fill in that data.
      ;; These cases can be differentiated by the type attribute of the iq tag:
      ;; if type='result', we asked for the whole roster.  If type='set', we are
      ;; getting a "roster push".
      (when (listp (car (xml-node-children (jabber-iq-query xml-data))))
	(dolist (item (xml-get-children (jabber-iq-query xml-data) 'item))
	  (let ((roster-item)
		(jid (intern (jabber-jid-user (xml-get-attribute item 'jid)) jabber-jid-obarray)))

	    ;; Find contact if already in roster
	    (setq roster-item (car (memq jid *jabber-roster*)))

	    ;; If not found, create a new roster item.
	    (when (null roster-item)
	      (message "%s added to roster" jid)
	      (setq roster-item jid)
	      (setq *jabber-roster* (cons roster-item *jabber-roster*)))

	    ;; Now, get all data associated with the contact.
	    (put roster-item 'name (jabber-xml-get-attribute item 'name))
	    (put roster-item 'subscription (jabber-xml-get-attribute item 'subscription))
	    (put roster-item 'ask (jabber-xml-get-attribute item 'ask))

	    ;; Since roster items can't be changed incrementally, we save the original XML
	    ;; to be able to modify it, instead of having to reproduce it.  This is for
	    ;; forwards compatibility.
	    (put roster-item 'xml item)

	    ;; xml-parse-tag will put "" as the only child element of an empty element,
	    ;; (e.g. <item jid="foo@bar"/> as opposed to <item jid="foo@bar"><group>baz</group></item>)
	    ;; which xml-get-children subsequently will choke on.  We want to avoid
	    ;; that with an extra check.
	    (if (listp (car (xml-node-children item)))
		(put roster-item 'groups (mapcar (lambda (foo) (nth 2 foo)) (xml-get-children item 'group)))
	      (put roster-item 'groups nil))

	    ;; If subscripton="remove", contact is to be removed from roster
	    (when (string= (get roster-item 'subscription) "remove")
	      (message "%s removed from roster" jid)
	      (setq *jabber-roster* (delq roster-item *jabber-roster*)))

	    )))
      (jabber-display-roster)
      (if (and id (string= type "set"))
	  (jabber-send-iq jabber-server "result" nil
			  nil nil nil nil id)))))

(defun jabber-process-data (xml-data closure-data)
  "Process random results from various requests."
  (let ((from (xml-get-attribute xml-data 'from))
	(xmlns (jabber-iq-xmlns xml-data))
	(type (jabber-xml-get-attribute xml-data 'type)))
    (with-current-buffer (get-buffer-create (concat "*-jabber-browse-:-" from "-*"))
      (setq buffer-read-only nil)
      (goto-char (point-max))

      (insert (propertize (xml-get-attribute xml-data 'from)
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

      (jabber-browse-mode)
      (run-hook-with-args 'jabber-alert-info-message-hooks 'browse (current-buffer) (funcall jabber-alert-info-message-function 'browse (current-buffer))))))

(defun jabber-process-browse (xml-data)
  "Handle results from jabber:iq:browse requests."
  (dolist (item (xml-node-children xml-data))
    (when (and (listp item)
	       (not (eq (xml-node-name item) 'ns)))
      (let ((jid (xml-get-attribute item 'jid))
	    (beginning (point)))
	(cond
	 ((or
	   (eq (xml-node-name item) 'user)
	   (string= (xml-get-attribute item 'category) "user"))
	  (insert (propertize "$ USER"
			      'face 'jabber-title-medium)
		  "\n\n"))
	 ((or
	   (eq (xml-node-name item) 'service)
	   (string= (xml-get-attribute item 'category) "service"))
	  (insert (propertize "* SERVICE"
			      'face 'jabber-title-medium)
		  "\n\n"))
	 ((or
	   (eq (xml-node-name item) 'conference)
	   (string= (xml-get-attribute item 'category) "conference"))
	  (insert (propertize "@ CONFERENCE"
			      'face 'jabber-title-medium)
		  "\n\n"))
	 (t
	  ;; So far I've seen "server" and "directory", both in the node-name.
	  ;; Those are actually service disco categories, but jabberd 2 seems
	  ;; to use them for browse results as well.  It's not right (as in
	  ;; JEP-0011), but it's reasonable.
	  (let ((category (xml-get-attribute item 'category)))
	    (if (= (length category) 0)
		(setq category (xml-node-name item)))
	    (insert (propertize (format "! OTHER: %s" category)
				'face 'jabber-title-medium)
		    "\n\n"))))
	(dolist (attr '((type . "Type:\t\t")
			(jid . "JID:\t\t")
			(name . "Name:\t\t")
			(version . "Version:\t")))
	  (let ((data (xml-get-attribute item (car attr))))
	    (if (> (length data) 0)
		(insert (cdr attr) (jabber-unescape-xml data) "\n"))))

	(if (listp (car (xml-node-children item)))
	    (dolist (ns (xml-get-children item 'ns))
	      (if (stringp (car (xml-node-children ns)))
		  (insert "Namespace:\t" (car (xml-node-children ns)) "\n"))))

	(put-text-property beginning (point) 'jabber-jid jid)
	(insert "\n\n")

	;; XXX: Is this kind of recursion really needed?
	(if (listp (car (xml-node-children item)))
	    (jabber-process-browse item))))))

(defun jabber-process-disco-info (xml-data)
  "Handle results from info disco requests."

  (let ((beginning (point)))
    (dolist (x (xml-node-children (jabber-iq-query xml-data)))
      (cond
       ((eq (xml-node-name x) 'identity)
	(let ((name (jabber-xml-get-attribute x 'name))
	      (category (jabber-xml-get-attribute x 'category))
	      (type (jabber-xml-get-attribute x 'type)))
	  (insert (propertize (if name
				  (jabber-unescape-xml name)
				"Unnamed") ; tsk, tsk... name is _required_
			      'face 'jabber-title-medium)
		  "\n\nCategory:\t" category "\n")
	  (if type
	      (insert "Type:\t\t" type "\n"))
	  (insert "\n")))
       ((eq (xml-node-name x) 'feature)
	(let ((var (jabber-xml-get-attribute x 'var)))
	  (insert "Feature:\t" var "\n")))))
    (put-text-property beginning (point) 'jabber-jid (jabber-xml-get-attribute xml-data 'from))))

(defun jabber-process-disco-items (xml-data)
  "Handle results from items disco requests."

  (if (and (car (xml-node-children (jabber-iq-query xml-data)))
	   (listp (car (xml-node-children (jabber-iq-query xml-data)))))
      (dolist (item (xml-get-children (jabber-iq-query xml-data) 'item))
	(let ((jid (jabber-xml-get-attribute item 'jid))
	      (name (jabber-xml-get-attribute item 'name))
	      (node (jabber-xml-get-attribute item 'node)))
	  (insert 
	   (propertize 
	    (concat
	     (propertize
	      (concat jid "\n" (if node (format "Node: %s\n" node)))
	      'face 'jabber-title-medium)
	     (jabber-unescape-xml name) "\n\n")
	    'jabber-jid jid
	    'jabber-node node))))
    (insert "No items found.\n")))

(defun jabber-process-version (xml-data)
  "Handle results from jabber:iq:version requests."
  
  (let ((query (jabber-iq-query xml-data)))
    (dolist (x '((name . "Name:\t\t") (version . "Version:\t") (os . "OS:\t\t")))
      (let ((data (car (xml-node-children (car (xml-get-children query (car x)))))))
	(when data
	  (insert (cdr x) data "\n"))))))

(defun jabber-return-version (xml-data)
  "Return client version as defined in JEP-0092.  Sender and ID are
determined from the incoming packet passed in XML-DATA."
  ;; Things we might check: does this iq message really have type='get' and
  ;; exactly one child, namely query with xmlns='jabber:iq:version'?
  ;; Then again, jabber-process-iq should take care of that.
  (let ((to (xml-get-attribute xml-data 'from))
	(id (xml-get-attribute xml-data 'id)))
    (jabber-send-iq to "result"
		    `(query ((xmlns . "jabber:iq:version"))
			    (name () "jabber.el")
			    (version () "0.4")
			    ;; Booting... /vmemacs.el
			    ;; Shamelessly stolen from someone's sig.
			    (os () ,(jabber-escape-xml (emacs-version))))
		    nil nil nil nil
		    id)))

(defun jabber-return-disco-info (xml-data)
  "Respond to a service discovery request.
See JEP-0030."
  (let ((to (xml-get-attribute xml-data 'from))
	(id (xml-get-attribute xml-data 'id)))
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

(defun jabber-do-register (xml-data closure-data)
  "Register new account with a Jabber server.
Call upon receiving \"result\" response to an \"jabber:iq:register\" get
request."
  ;; This should be implemented with widgets under jabber-process-data,
  ;; and it should support registering with other things than your own
  ;; jabber server (e.g. JUDs).  It should probably support jabber:x:data
  ;; too.
  (setq jabber-register-p nil)
  (let* ((query (jabber-iq-query xml-data))
	 (instructions (car (xml-node-children (car (xml-get-children query 'instructions)))))
	 (registered (xml-get-children xml-data 'registered))
	 (form nil))
    (if registered
	(progn
	  (message "%s@%s is already registered." jabber-username jabber-server)
	  (sit-for 2))
      (message "Registration instructions: %s" instructions)
      (sit-for 5)
      (dolist (x (xml-node-children query))
	(cond
	 ((eq (xml-node-name x) 'instructions)
	  ;; already handled
	  )
	 ((eq (xml-node-name x) 'username)
	  (message "Using %s as username" jabber-username)
	  (setq form (cons `(username nil ,jabber-username) form))
	  (sit-for 2))
	 ((eq (xml-node-name x) 'password)
	  (setq form (cons `(password nil ,(jabber-read-passwd)) form)))
	 (t
	  (setq form (cons `(,x nil ,(read-string (format "%s: " x))) form)))))
      (jabber-send-iq jabber-server
		      "set"
		      `(query ((xmlns . "jabber:iq:register"))
			      ,form)
		      #'jabber-process-register 'success
		      #'jabber-process-register 'error))))
	    
(defun jabber-do-logon (xml-data closure-data)
  "send username and password in logon attempt"
  (cond
   ((string= (xml-get-attribute xml-data 'type) "result")
    (let (auth)
      (if (xml-get-children (jabber-iq-query xml-data) 'digest)
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
			  #'jabber-process-logon nil
			  #'jabber-report-success "Logon")
	(jabber-disconnect))))
   (t
    (error "Logon error ended up the wrong place"))))
	
(defun jabber-process-logon (xml-data closure-data)
  "receive login success, and request roster."
  (cond 
   ((string= (xml-get-attribute xml-data 'type) "result")
    (jabber-send-iq jabber-server
                    "get" 
                    '(query ((xmlns . "jabber:iq:roster")))
                    #'jabber-process-roster 'initial
		    #'jabber-report-success "Roster retrieval")

    ;; You are by no means forced to send presence when connected.
    ;;(jabber-send-sexp '((presence)))
    )
   (t
    (error "Logon error ended up in the wrong place"))))

(defun jabber-process-register (xml-data closure-data)
  "Receive registration success of failure.
CLOSURE-DATA is either 'success or 'error."
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

(defun jabber-process-iq (xml-data)
  "process an incoming iq stanza"
  (let* ((id (xml-get-attribute xml-data 'id))
         (type (xml-get-attribute xml-data 'type))
         (from (xml-get-attribute xml-data 'from))
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
      (let ((handler (cdr (assoc (xml-get-attribute query 'xmlns) jabber-iq-get-xmlns-alist))))
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
      (let ((handler (cdr (assoc (xml-get-attribute query 'xmlns) jabber-iq-set-xmlns-alist))))
	(if handler
	    (funcall handler xml-data)
	  (jabber-send-sexp `(iq ((to . ,from)
				  (type . "error")
				  (id . ,id))
				 (error ((type . "cancel"))
					(feature-not-implemented
					 ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))))))))))))


(defun jabber-process-input (xml-data)
  "process an incoming parsed tag"
  (let ((tag (xml-node-name xml-data)))
     (cond
      ((eq tag 'iq)
       (jabber-process-iq xml-data))
      
      ((eq tag 'message)
       (let ((from (xml-get-attribute xml-data 'from))
             (type (xml-get-attribute xml-data 'type))
             (subject (if (xml-get-children xml-data 'subject)
                          (car (xml-node-children (car (xml-get-children xml-data 'subject))))))
             (body (if (xml-get-children xml-data 'body)
                       (car (xml-node-children (car (xml-get-children xml-data 'body))))))
             (thread (if (xml-get-children xml-data 'thread)
			 (car (xml-node-children (car (xml-get-children xml-data 'thread)))))))
         (jabber-process-message from subject body thread type)))

      ((eq tag 'presence)
       (let ((from (xml-get-attribute xml-data 'from))
             (to (xml-get-attribute xml-data 'to))
             (type (xml-get-attribute xml-data 'type))
             (show (if (listp (car (xml-node-children xml-data)))
                       (car (xml-node-children (car (xml-get-children xml-data 'show))))))
             (status (if (listp (car (xml-node-children xml-data)))
                         (car (xml-node-children (car (xml-get-children xml-data 'status))))))
	     (priority (or (if (listp (car (xml-node-children xml-data)))
			       (car (xml-node-children (car (xml-get-children xml-data 'priority)))))
			   "")))
         (jabber-process-presence from to show status type (string-to-number priority)))))))
   

(defun jabber-filter (process string)
  "the filter function for the jabber process"
  (cond
   ((string-match "</stream:stream>" string)
    (jabber-disconnect))
   ((string-match "<stream:stream" string)
    (setq jabber-session-id
          (progn (string-match "id='\\([A-Za-z0-9]+\\)'" string)
               (match-string 1 string)))
    ;; Now proceed with logon.
    (if jabber-register-p
	(jabber-get-register jabber-server)
      (jabber-get-auth jabber-server)))
   (t
    (if (active-minibuffer-window)
        (run-with-idle-timer 0.01 nil #'jabber-filter process string)
      (with-temp-buffer
        (setq *xmlq* (concat *xmlq* string))
        (if (string-match " \\w+=''" *xmlq*)
            (replace-match ""))
        (catch 'jabber-no-tag
          (while (string-match "<\\([a-zA-Z0-9\:]+\\)\\s-" *xmlq*)
            (if (or (string-match (concat "<" (match-string 1 *xmlq*) "[^<>]*?/>") *xmlq*)
                    (string-match (concat "<" (match-string 1 *xmlq*) ".*?>[^\0]+?</" (match-string 1 *xmlq*) ">") *xmlq*))
                (progn
                  (insert (match-string 0 *xmlq*))
                  (goto-char (point-min))
                  (setq *xmlq* (substring *xmlq* (match-end 0)))
                  (let ((xml-data (xml-parse-region (point-min)
                                                    (point-max))))
                    (if xml-data
                        (progn
                          (if jabber-debug
			      (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
				(goto-char (point-max))
				(insert (format "receive %S\n\n" (car xml-data)))))
                          (jabber-process-input (car xml-data)))))
                  (erase-buffer))
              (throw 'jabber-no-tag t)))))))))

(defun jabber-read-passwd ()
  "Read Jabber password, either from customized variable or from minibuffer.
See `jabber-password'."
  (or jabber-password (read-passwd "Jabber password: ")))

(defun jabber-clear-roster ()
  "Clean up the roster.
This is made complicated by the fact that the JIDs are symbols with properties."
  (mapatoms (lambda (x)
	      (unintern x jabber-jid-obarray))
	    jabber-jid-obarray)
  (setq *jabber-roster* nil))

(defun jabber-connect (&optional registerp)
  "connect to the jabber server and start a jabber xml stream
With prefix argument, register a new account."
  (interactive "p")
  (if *jabber-connected*
      (message "Already connected")
    (setq *xmlq* "")
    (jabber-clear-roster)
    (setq *jabber-current-priority* jabber-default-priority)
    (let ((coding-system-for-read 'utf-8)
	  (coding-system-for-write 'utf-8))
      (setq *jabber-connection* (open-network-stream "jabber"
						     "*-jabber-*"
						     jabber-server
						     jabber-port)))
    (set-process-filter *jabber-connection* #'jabber-filter)
    (set-process-sentinel *jabber-connection* #'jabber-sentinel)

    (setq jabber-register-p (> registerp 1))
    (process-send-string *jabber-connection*
			 (concat "<?xml version='1.0'?><stream:stream to='" 
				 jabber-server 
				 "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>"))
    ;; Next thing happening is the server sending its own <stream:stream> start tag.
    ;; That is handled in jabber-filter.

    (setq *jabber-connected* t)))

(defun jabber-sentinel (process event)
  "alert user about lost connection"
  (beep)
  (message "Jabber connection lost: `%s'" event)
  (jabber-disconnect))

(defun jabber-send-groupchat (group body)
  "send a message to a groupchat"
  (jabber-send-message group body nil "groupchat"))

(defun jabber-send-chat (to body)
  "send a chat message to someone"
  (jabber-send-message to body nil "chat"))

(defun jabber-send-subscription-request (to &optional request)
  "send a subscription request to jid, showing him your request text, if specified"
  (interactive (list (jabber-read-jid-completing "to: ")
		     (read-string "request: " nil nil nil t)))
  (jabber-send-sexp `(presence ((to . ,to)
                                (type . "subscribe"))
                               ,(if (and request (> (length request) 0))
                                   request))))

(defun jabber-send-message (to body subject type)
  "send a message tag to the server"
  (interactive (list (jabber-read-jid-completing "to: ")
		     (read-string "body: " nil nil nil t)
		     (read-string "subject: " nil nil nil t)
		     (read-string "type: " nil nil nil t)))
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
		 (list jid (read-string "Name: " nil nil name t)
		       (read-from-minibuffer "Groups: " nil nil t nil (format "%S" groups) t))))
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
  (jabber-send-sexp `(presence ((to . ,group)
                                (type . "unavailable")))))

(defun jabber-groupchat-join (group nickname)
  "join a groupchat"
  (interactive (list (read-string "group: ")
		     (read-string (format "Nickname: (default %s) "
					  jabber-nickname) 
				  nil nil jabber-nickname t)))
  (jabber-send-sexp `(presence ((to . ,(format "%s/%s" group nickname)))))

  (if (not (member group *jabber-active-groupchats*))
      (setq *jabber-active-groupchats* (cons group *jabber-active-groupchats*)))
  (jabber-groupchat-display group))

(defun jabber-send-presence (show status priority)
  "send a presence tag to the server"
  (interactive (list (completing-read "show:"
				      '(("" . nil) ("away" . nil) ("xa" . nil) ("dnd" . nil) ("chat" . nil)) nil t)
		     (read-string "status: " *jabber-current-status* '*jabber-status-history* nil t)
		     (read-string "priority: " (int-to-string *jabber-current-priority*))))
  (setq *jabber-current-status* status)
  (setq *jabber-current-show* show)
  (setq *jabber-current-priority* (string-to-int priority))
  (jabber-send-sexp `(presence ()
                               ,(if (> (length status) 0)
                                    `(status () ,(jabber-escape-xml status)))
			       ,(if (> (length show) 0)
                                    `(show () ,(jabber-escape-xml show)))
			       (priority () ,(jabber-escape-xml (int-to-string *jabber-current-priority*)))))
  (jabber-display-roster))

(defun jabber-send-iq (to type query success-callback success-closure-data
			  error-callback error-closure-data &optional result-id)
  "Send an iq stanza to the specified entity, and optionally set up a callback.
TO is the addressee.
TYPE is one of \"get\", \"set\", \"result\" or \"error\".
QUERY is a list containing the child of the iq node in the format `sexp2xml'
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
  (jabber-send-iq to
		  "get"
		  '(query ((xmlns . "jabber:iq:register")))
		  #'jabber-do-register nil
		  #'jabber-report-success "Account registration"))

(defun jabber-get-browse (to)
  "send a browse infoquery request to someone"
  (interactive (list (jabber-read-jid-completing "browse: ")))
  (jabber-send-iq to 
                  "get"
                  '(query ((xmlns . "jabber:iq:browse")))
                  #'jabber-process-data #'jabber-process-browse
		  #'jabber-process-data "Browse failed"))

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

(defun jabber-disconnect ()
  "disconnect from the jabber server and re-initialise the jabber package variables"
  (interactive)
  (when (eq (process-status *jabber-connection*) 'open)
    (process-send-string *jabber-connection* "</stream:stream>")
    (delete-process *jabber-connection*))
  (kill-buffer (process-buffer *jabber-connection*))
  (jabber-clear-roster)
  (setq *xmlq* "")
  (setq *jabber-connected* nil)
  (setq *jabber-active-groupchats* nil)
  (setq jabber-session-id nil))

;; Alert hooks

;; Message alert hooks
(defun jabber-message-default-message (from buffer text)
  (format "Message from %s" (jabber-jid-displayname from)))

(defun jabber-message-beep (from buffer text proposed-alert)
  "Beep when a message arrives"
  (beep))

(defun jabber-message-echo (from buffer text proposed-alert)
  "Show a message in the echo area when a message arrives"
  (if proposed-alert
      (message "%s" proposed-alert)))

(defun jabber-message-wave (from buffer text proposed-alert)
  "Play the wave file specified in `jabber-alert-message-wave'"
  (jabber-play-sound-file jabber-alert-message-wave))

(defun jabber-message-switch (from buffer text proposed-alert)
  "Switch to the buffer where a new message has arrived."
  (switch-to-buffer buffer))

(defun jabber-message-ratpoison (from buffer text proposed-alert)
  "Show a message through the Ratpoison window manager"
  (if proposed-alert
      (call-process "ratpoison" nil nil nil "-c" (format "echo %s" proposed-alert))))

(defun jabber-message-screen (from buffer text proposed-alert)
  "Show a message through the Screen terminal manager"
  (if proposed-alert
      (call-process "screen" nil nil nil "-X" "echo" proposed-alert)))

;; Presence alert hooks
(defun jabber-presence-default-message (who oldstatus newstatus statustext)
  "This function returns nil if OLDSTATUS and NEWSTATUS are equal, and in other
cases a string of the form \"'name' (jid) is now NEWSTATUS (STATUSTEXT)\".

This function is not called directly, but is the default for
`jabber-alert-presence-message-function'."
  (cond
   ((equal oldstatus newstatus)
      nil)
   (t
    (let ((formattedname
	   (if (> (length (get who 'name)) 0)
	       (get who 'name)
	     (symbol-name who)))
	  (formattedstatus
	   (or
	    (cdr (assoc newstatus '(("subscribe" . " requests subscription to your presence")
				    ("subscribed" . " has granted presence subscription to you")
				    ("unsubscribe" . " no longer subscribes to your presence")
				    ("unsubscribed" . " cancels your presence subscription"))))
	    (concat " is now "
		    (or
		     (cdr (assoc newstatus jabber-presence-strings))
		     newstatus))))
	  (formattedtext
	   (if (> (length statustext) 0)
	       (concat " (" (jabber-unescape-xml statustext) ")")
	     "")))
      (concat formattedname formattedstatus formattedtext)))))

(defun jabber-presence-beep (who oldstatus newstatus statustext proposed-alert)
  "Beep when someone's presence changes"
  (if proposed-alert
      (beep)))

(defun jabber-presence-echo (who oldstatus newstatus statustext proposed-alert)
  "Show a message in the echo area"
  (if proposed-alert
      (message "%s" proposed-alert)))

(defun jabber-presence-wave (who oldstatus newstatus statustext proposed-alert)
  "Play the wave file specified in `jabber-alert-presence-wave'"
  (if proposed-alert
      (jabber-play-sound-file jabber-alert-presence-wave)))

(defun jabber-presence-update-roster (who oldstatus newstatus statustext proposed-alert)
  "Update the roster display by calling `jabber-display-roster'"
  (jabber-display-roster))

(defun jabber-presence-switch (who oldstatus newstatus statustext proposed-alert)
  "Switch to the roster buffer"
  (switch-to-buffer (process-buffer *jabber-connection*)))

(defun jabber-presence-ratpoison (who oldstatus newstatus statustext proposed-alert)
  "Show a message through the Ratpoison window manager"

  (if proposed-alert
      (call-process "ratpoison" nil nil nil "-c" (concat "echo " proposed-alert))))

(defun jabber-presence-screen (who oldstatus newstatus statustext proposed-alert)
  "Show a message through the Screen terminal manager"

  (if proposed-alert
      (call-process "screen" nil nil nil "-X" "echo" proposed-alert)))

;;; Info alert hooks

(defun jabber-info-default-message (infotype buffer)
  "Function for constructing info alert messages.

The argument is INFOTYPE, a symbol telling the kind of info request completed.
This function uses `jabber-info-message-alist' to find a message."
  (concat (cdr (assq infotype jabber-info-message-alist))
	  " (buffer "(buffer-name buffer) ")"))

(defun jabber-info-echo (infotype buffer proposed-alert)
  "Show a message in the echo area"
  (if proposed-alert
	(message "%s" proposed-alert)))

(defun jabber-info-beep (infotype buffer proposed-alert)
  "Beep on completed info requests"
  (if proposed-alert
      (beep)))

(defun jabber-info-wave (infotype buffer proposed-alert)
  "Play the wave file specified in `jabber-alert-info-wave'"
  (if proposed-alert
      (jabber-play-sound-file jabber-alert-info-wave)))

(defun jabber-info-ratpoison (infotype buffer proposed-alert)
  "Show a message through the Ratpoison window manager"
  (if proposed-alert
      (call-process "ratpoison" nil nil nil "-c" (concat "echo " proposed-alert))))

(defun jabber-info-screen (infotype buffer proposed-alert)
  "Show a message through the Screen terminal manager"
  (if proposed-alert
      (call-process "screen" nil nil nil "-X" "echo" proposed-alert)))

(defun jabber-info-switch (infotype buffer proposed-alert)
  "Switch to buffer of completed request"
  (switch-to-buffer buffer))

(provide 'jabber)
