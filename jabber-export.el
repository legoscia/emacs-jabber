;;; jabber-export.el --- export Jabber roster to file

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

(defun jabber-export-roster (file)
  "Export roster to FILE."
  (interactive "FExport roster to file: ")
  (with-temp-file file
    (insert "<iq><query xmlns='jabber:iq:roster'>\n")
    (dolist (item *jabber-roster*)
      (insert (jabber-sexp2xml (get item 'xml)) "\n"))
    (insert "</query></iq>\n")))

(provide 'jabber-export)

;;; arch-tag: 9c6b94a9-290a-4c0f-9286-72bd9c1fb8a3
