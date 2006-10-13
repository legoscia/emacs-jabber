;;; jabber-vcard-avatars.el --- Avatars by JEP-0153

;; Copyright (C) 2006  Magnus Henoch

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(require 'jabber-avatar)

(defcustom jabber-vcard-avatars-retrieve (and (fboundp 'display-images-p)
					      (display-images-p))
  "Automatically download vCard avatars?"
  :group 'jabber-avatar
  :type 'boolean)

(defcustom jabber-vcard-avatars-publish t
  "Publish your vCard photo as avatar?"
  :group 'jabber-avatar
  :type 'boolean)

(defvar jabber-vcard-avatars-current-hash nil
  "SHA1 hash of avatar currently published through presence.")

(add-to-list 'jabber-presence-chain 'jabber-vcard-avatars-presence)
(defun jabber-vcard-avatars-presence (xml-data)
  "Look for vCard avatar mark in <presence/> stanza."
  ;; Only look at ordinary presence
  (when (and jabber-vcard-avatars-retrieve
	     (null (jabber-xml-get-attribute xml-data 'type)))
    (let* ((from (jabber-jid-user (jabber-xml-get-attribute xml-data 'from)))
	   (photo (jabber-xml-path xml-data '(("vcard-temp:x:update" . "x") photo)))
	   (sha1-hash (car (jabber-xml-node-children photo))))
      (if (null sha1-hash)
	  ;; User has removed avatar
	  (jabber-avatar-set from nil)
	(if (jabber-avatar-find-cached sha1-hash)
	    ;; Avatar is cached
	    (jabber-avatar-set from sha1-hash)
	  ;; Avatar is not cached; retrieve it
	  (jabber-vcard-avatars-fetch from))))))

(defun jabber-vcard-avatars-vcard (iq from)
  "Get the photo from the vCard, and set the avatar."
  (let ((photo (assq 'PHOTO (jabber-vcard-parse (jabber-iq-query iq)))))
    (if photo
	(let ((avatar (jabber-avatar-from-base64-string (nth 2 photo)
							(nth 1 photo))))
	  (jabber-avatar-cache avatar)
	  (jabber-avatar-set from avatar))
      (jabber-avatar-set from nil))))

(defun jabber-vcard-avatars-fetch (who)
  "Fetch WHO's vCard, and extract avatar."
  (interactive (list (jabber-read-jid-completing "Fetch whose vCard avatar: ")))
  (jabber-send-iq who "get" '(vCard ((xmlns . "vcard-temp")))
		  #'jabber-vcard-avatars-vcard who
		  #'ignore nil))

(add-hook 'jabber-post-connect-hook 'jabber-vcard-avatars-find-current)
(defun jabber-vcard-avatars-find-current ()
  "Request our own vCard, to find hash of avatar."
  (when jabber-vcard-avatars-publish
    (jabber-send-iq nil "get" '(vCard ((xmlns . "vcard-temp")))
		    #'jabber-vcard-avatars-find-current-1 t
		    #'jabber-vcard-avatars-find-current-1 nil)))

(defun jabber-vcard-avatars-find-current-1 (xml-data success)
  (jabber-vcard-avatars-update-current
   (and success
	(let ((photo (assq 'PHOTO (jabber-vcard-parse (jabber-iq-query xml-data)))))
	  (when photo
	    (let ((avatar (jabber-avatar-from-base64-string (nth 2 photo)
							    (nth 1 photo))))
	      (avatar-sha1-sum avatar)))))))

(defun jabber-vcard-avatars-update-current (new-hash)
  (let ((old-hash jabber-vcard-avatars-current-hash))
    (when (not (string= old-hash new-hash))
      (setq jabber-vcard-avatars-current-hash new-hash)
      (jabber-vcard-avatars-send-presence))))

(defun jabber-vcard-avatars-send-presence ()
  (jabber-send-presence *jabber-current-show* *jabber-current-status* *jabber-current-priority*))

(add-to-list 'jabber-presence-element-functions 'jabber-vcard-avatars-presence-element)
(defun jabber-vcard-avatars-presence-element ()
  (when jabber-vcard-avatars-publish
    (list
     `(x ((xmlns . "vcard-temp:x:update"))
	 ;; if "not yet ready to advertise image", don't.
	 ;; that is, we haven't yet checked what avatar we have.
	 ,(when jabber-vcard-avatars-current-hash
	    `(photo () ,jabber-vcard-avatars-current-hash))))))
	     
(provide 'jabber-vcard-avatars)
;; arch-tag: 3e50d460-8eae-11da-826c-000a95c2fcd0
