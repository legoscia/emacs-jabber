;; jabber-keymap.el - common keymap for many modes

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

(defvar jabber-common-keymap nil)

(unless jabber-common-keymap
  (setq jabber-common-keymap (make-sparse-keymap))
  (define-key jabber-common-keymap "\C-c\C-c" 'jabber-popup-chat-menu)
  (define-key jabber-common-keymap "\C-c\C-r" 'jabber-popup-roster-menu)
  (define-key jabber-common-keymap "\C-c\C-i" 'jabber-popup-info-menu)
  (define-key jabber-common-keymap "\C-c\C-m" 'jabber-popup-muc-menu)
  (define-key jabber-common-keymap "\C-c\C-s" 'jabber-popup-service-menu)
  (define-key jabber-common-keymap [mouse-2] 'jabber-popup-combined-menu)
  )

(provide 'jabber-keymap)

;;; arch-tag: 22a9993d-a4a7-40ef-a025-7cff6c3f5587
