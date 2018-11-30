;; jabber-macos-notify.el - emacs-jabber interface to macOS notify

;; Copyright (C) 2018 - Kirill A. Korinsky - kirill@korins.ky

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

(eval-when-compile (require 'jabber-alert))

(defcustom jabber-macos-notify-message-header "Jabber message"
  "Defines the header of the pop up."
  :type 'string
  :group 'jabber-alerts)

(defcustom jabber-macos-notify-app "Emacs Jabber"
  "Defines the app of the pop up."
  :type 'string
  :group 'jabber-alerts)

(defcustom jabber-macos-notify-urgency "low"
  "Urgency of macos-notify message"
  :type '(choice (const :tag "Low" "low")
                 (const :tag "Normal" "normal")
                 (const :tag "Critical" "critical"))
  :group 'jabber-alerts)

(defcustom jabber-macos-notify-method 'applescript
  "Specifies the method for macos notify call."
  :type '(choice (const :tag "Apple Script" applescript))
  ;; possible add support https://github.com/julienXX/terminal-notifier or something more rich with icon :)
  :group 'jabber-alerts)

(defun jabber-macos-notify-message (text &optional title)
  "Show MSG using macos-notify"
  (let
      ((body (or (jabber-escape-xml text) " "))
       (head (jabber-escape-xml
              (or title
                  (or jabber-macos-notify-message-header " ")
                  text))))
    ;; Possible errors include not finding the notify-send binary.
    (condition-case e
        (cond
         ((eq jabber-macos-notify-method 'applescript)
          (do-applescript (format "display notification %S with title %S" body head))
      (error nil))))

(define-jabber-alert macos-notify "Show a message through the macos notification center"
  'jabber-macos-notify-message)

(define-personal-jabber-alert jabber-muc-macos-notify)

(provide 'jabber-macos-notify)
