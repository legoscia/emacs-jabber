;;; jabber-avatar.el --- generic functions for avatars

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

;; There are several methods for transporting avatars in Jabber
;; (JEP-0008, JEP-0084, JEP-0153).  They all have in common that they
;; identify avatars by their SHA1 checksum, and (at least partially)
;; use Base64-encoded image data.  Thus this library of support
;; functions for interpreting and caching avatars.

;; A contact with an avatar has the image in the avatar property of
;; the JID symbol.  Use `jabber-avatar-set' to set it.

;;; Code:

(eval-when-compile (require 'cl))

;;;; Variables

(defgroup jabber-avatar nil
  "Avatar related settings"
  :group 'jabber)

(defcustom jabber-avatar-cache-directory "~/.jabber-avatars/"
  "Directory to use for cached avatars"
  :group 'jabber-avatar
  :type 'directory)

;;;; Avatar data handling

(defstruct avatar sha1-sum mime-type url base64-data height width bytes)

(defun jabber-avatar-from-url (url)
  "Construct an avatar structure from the given URL.
Retrieves the image to find info about it."
  (with-current-buffer (let ((coding-system-for-read 'binary))
			 (url-retrieve-synchronously url))
    (let* ((case-fold-search t)
	   (mime-type (ignore-errors
			(search-forward-regexp "^content-type:[ \t]*\\(.*\\)$")
			(match-string 1)))
	   (data (progn
		   (search-forward "\n\n")
		   (buffer-substring (point) (point-max)))))
      (prog1
	  (jabber-avatar-from-data data nil mime-type)
	(kill-buffer nil)))))

(defun jabber-avatar-from-file (filename)
  "Construct an avatar structure from FILENAME."
  (require 'mailcap)
  (let ((data (with-temp-buffer
		(insert-file-contents-literally filename)
		(buffer-string)))
	(mime-type (progn (string-match "\\.[^.]+$" filename)
			  (mailcap-extension-to-mime (match-string 0 filename)))))
    (jabber-avatar-from-data data nil mime-type)))

(defun jabber-avatar-from-base64-string (base64-string &optional mime-type)
  "Construct an avatar stucture from BASE64-STRING.
If MIME-TYPE is not specified, try to find it from the image data."
  (jabber-avatar-from-data nil base64-string mime-type))

(defun jabber-avatar-from-data (raw-data base64-string &optional mime-type)
  "Construct an avatar structure from RAW-DATA and/or BASE64-STRING.
If either is not provided, it is computed.
If MIME-TYPE is not specified, try to find it from the image data."
  (let* ((data (or raw-data (base64-decode-string base64-string)))
	 (bytes (length data))
	 (sha1-sum (sha1 data))
	 (base64-data (or base64-string (base64-encode-string raw-data)))
	 (type (or mime-type
		   (cdr (assq (get :type (cdr (create-image data nil t)))
			      '((png "image/png")
				(jpeg "image/jpeg")
				(gif "image/gif")))))))
    (jabber-avatar-compute-size
     (make-avatar :mime-type mime-type :sha1-sum sha1-sum :base64-data base64-data :bytes bytes))))

;; XXX: This function is based on an outdated version of JEP-0084.
;; (defun jabber-avatar-from-data-node (data-node)
;;   "Construct an avatar structure from the given <data/> node."
;;   (jabber-xml-let-attributes
;;    (content-type id bytes height width) data-node
;;    (let ((base64-data (car (jabber-xml-node-children data-node))))
;;      (make-avatar :mime-type content-type :sha1-sum id :bytes bytes
;; 		  :height height :width width :base64-data base64-data))))

(defun jabber-avatar-image (avatar)
  "Create an image from AVATAR."
  (create-image (with-temp-buffer
		  (set-buffer-multibyte nil)
		  (insert (avatar-base64-data avatar))
		  (base64-decode-region (point-min) (point-max))
		  (buffer-string))
		nil
		t))

(defun jabber-avatar-compute-size (avatar)
  "Compute and set the width and height fields of AVATAR.
Return AVATAR."
  ;; XXX: don't call image-size if not X
  (let ((size (image-size (jabber-avatar-image avatar) t)))
    (setf (avatar-width avatar) (car size))
    (setf (avatar-height avatar) (cdr size))
    avatar))

;;;; Avatar cache

(defun jabber-avatar-find-cached (sha1-sum)
  "Return file name of cached image for avatar identified by SHA1-SUM.
If there is no cached image, return nil."
  (car (file-expand-wildcards (concat (file-name-as-directory jabber-avatar-cache-directory)
				      sha1-sum
				      ".*"))))

(defun jabber-avatar-cache (avatar)
  "Cache the AVATAR."
  (let* ((id (avatar-sha1-sum avatar))
	 (base64-data (avatar-base64-data avatar))
	 (mime-type (avatar-mime-type avatar))
	 (extension
	  (cond
	   ((string= mime-type "image/png")
	    ".png")
	   ((string= mime-type "image/jpeg")
	    ".jpg")
	   ((string= mime-type "image/gif")
	    ".gif")
	   (t
	    ".dat")))
	 (filename (expand-file-name (concat id extension) jabber-avatar-cache-directory))
	 (buffer (create-file-buffer filename)))
    (unless (file-directory-p jabber-avatar-cache-directory)
      (make-directory jabber-avatar-cache-directory))

    (with-current-buffer buffer
      (let ((require-final-newline nil))
	(setq buffer-file-coding-system 'binary)
	(if (fboundp 'set-buffer-multibyte)
	    (set-buffer-multibyte nil))
	(set-visited-file-name filename t)
	(insert base64-data)
	(base64-decode-region (point-min) (point-max))
	(basic-save-buffer)))
    (kill-buffer buffer)))

;;;; Set avatar for contact

(defun jabber-avatar-set (jid avatar)
  "Set the avatar of JID to be AVATAR.
JID is a string containing a bare JID.
AVATAR may be one of:
* An avatar structure.
* The SHA1 sum of a cached avatar.
* nil, meaning no avatar."
  (cond
   ((avatar-p avatar)
    (put (jabber-jid-symbol jid) 
	 'avatar (jabber-avatar-image avatar)))
   ((stringp avatar)
    (put (jabber-jid-symbol jid)
	 'avatar (jabber-avatar-image
		  (jabber-avatar-from-file
		   (jabber-avatar-find-cached avatar)))))
   (t
    (put (jabber-jid-symbol jid)
	 'avatar nil)))

  (jabber-presence-update-roster (jabber-jid-symbol jid)))

(provide 'jabber-avatar)
;; arch-tag: 2405c3f8-8eaa-11da-826c-000a95c2fcd0
