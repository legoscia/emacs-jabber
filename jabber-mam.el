;; jabber-mam.el - XEP-0313 Message Archive Management

;; Copyright (C) 2017 - Thibault Marin - thibault.marin@gmx.com

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

;;; Commentary:

;; Provides an interface to server message archives following XEP-0313 (message
;; archive management--MAM).  To use, set `jabber-history-enabled' and
;; `jabber-history-mam' to non-nil values.  This requires server support for
;; XEP-0313 (http://xmpp.org/extensions/xep-0313.html) and proper configuration.
;; In particular, the archiving behavior can be configured to select which
;; messages are stored.  User preferences can be set by server requests
;; (http://xmpp.org/extensions/xep-0313.html#prefs).


;;; Code:

(require 'jabber-iq)

(defcustom jabber-mam-namespace "urn:xmpp:mam:0"
  "XMPP namespace for XEP-0313 request.
This can be determined by sending a request to the server as
described in http://xmpp.org/extensions/xep-0313.html#support."
  :type 'string
  :group 'jabber-history)

(defvar jabber-mam-results nil
  "Buffer receiving the archived messages from the server.")

(defvar jabber-mam-done nil
  "Flag raised when results paged over multiple sets have been processed.")

(defvar jabber-mam-lock nil
  "Synchronization variable to return MAM results synchronously.")

(defun jabber-mam-make-base-query (jid jid-with)
  "Build basic query requesting messages between JID and JID-WITH."
  `(x ((xmlns . "jabber:x:data")
       (type . "submit"))
      (field ((var . "FORM_TYPE")
              (type . "hidden"))
             (value () ,jabber-mam-namespace))
      (field ((var . "id"))
             (value () ,jid))
      (field ((var . "with"))
             (value () ,jid-with))))

(defun jabber-mam-make-query (jid jid-with start-time end-time number after)
  "Build request for server requesting archived messages.
Request messages between JID and JID-WITH between START-TIME and END-TIME
limiting to NUMBER results.  AFTER is used when paging through multiple result
sets: it contains the ID (returned by the server) for the last message in each
result set."
  (let* ((xxmlns (jabber-mam-make-base-query jid jid-with))
         (query `(query ((xmlns . ,jabber-mam-namespace)))))
    (when start-time
      (add-to-list 'xxmlns `(field ((var . "start"))
                                   (value (), (jabber-encode-time start-time)))
                   t))
    (when end-time
      ;; End time is offset by 1 second to avoid duplicate messages
      (add-to-list 'xxmlns `(field ((var . "end"))
                                   (value (), (jabber-encode-time
                                               (- end-time 1))))
                   t))
    (add-to-list 'query xxmlns t)
    (when (or number after)
      (let ((setxmlns '(set ((xmlns . "http://jabber.org/protocol/rsm")))))
        (when number
          ;; Limit number of results
          (add-to-list 'setxmlns `(max () ,(format "%d" number)) t)
          (add-to-list 'setxmlns '(before ()) t))
        (when after
          ;; Page through results (XMPP Result Set Management)
          (add-to-list 'setxmlns `(after () ,after) t))
      (add-to-list 'query setxmlns t)))
    query))

(defun jabber-mam-process-entry (mam-result)
  "Extract message information from MAM-RESULT and add to results list.
The output message information is stored in `jabber-mam-results'
in the same format as the one used by the file archive.  The
message is dropped if the function fails to fully extract the
message information (timestamp, from/to, body)."
  (let* ((mam-fwd (car (jabber-xml-get-children mam-result 'forwarded)))
         ;; Get <message> tag
         (mam-msg (when (jabber-xml-get-children mam-fwd 'message)
                    (car (jabber-xml-get-children mam-fwd 'message))))
         ;; Get timestamp
         (mam-stamp (when (jabber-xml-get-children mam-fwd 'delay)
                      (jabber-xml-get-attribute
                       (car (jabber-xml-get-children mam-fwd 'delay)) 'stamp)))
         ;; Get message body
         (mam-msg-body
          (when (and mam-msg (jabber-xml-get-children mam-msg 'body))
            (car (jabber-xml-get-children mam-msg 'body))))
         ;; Render message body
         (mam-msg-body-txt
          (when mam-msg-body
            (substring (format "%s" (cdr (cdr mam-msg-body))) 1 -1)))
         ;; Get <from> tag
         (mam-msg-from
          (when mam-msg
            (let ((mam-msg-from-t (jabber-jid-user (jabber-xml-get-attribute
                                                    mam-msg 'from))))
              (if (string= mam-msg-from-t mam-jid-me) "me"
                mam-msg-from-t))))
         ;; Get <to> tag
         (mam-msg-to
          (when mam-msg
            (let ((mam-msg-to-t (jabber-jid-user (jabber-xml-get-attribute
                                                  mam-msg 'to))))
              (if (string= mam-msg-to-t mam-jid-me) "me"
                mam-msg-to-t))))
         ;; Get message direction (from "me" or to "me")
         (mam-msg-dir (cond ((string= mam-msg-from "me") "out")
                            ((string= mam-msg-to "me") "in")
                            (t "me"))))
    (when (and mam-stamp mam-msg-dir mam-msg-from mam-msg-to mam-msg-body-txt)
      ;; Push to results list
      (push (vector
             mam-stamp mam-msg-dir mam-msg-from mam-msg-to mam-msg-body-txt)
            jabber-mam-results))))

(defun jabber-mam-process-fin (xml-data)
  "Process final server response from XML-DATA and determine the next action.
This function handles the server response corresponding to the
end of a result set.  If the <complete> tag is found, then no
subsequent query is required (`jabber-mam-done' is set to t).  If
the result set is not complete, the <last-id> tag is stored (in
`jabber-mam-last-id') and used to initialize a continuation
request.

In both cases, the lock (`jabber-mam-lock') is released for the caller
\('jabber-mam-query') to continue."
  (let* ((fin (jabber-xml-get-children xml-data 'fin))
         (complete (jabber-xml-get-attribute (car fin) 'complete))
         (set (jabber-xml-get-children (car fin) 'set))
         (last
          (when set (jabber-xml-get-children (car set) 'last)))
         (last-id (when last (cadr (cdr (car last))))))
    (if (and (or (not complete) (not (string= complete "true"))) last-id)
        ;; Result set is not complete, next request should start with
        ;; `last-id'
        (setq jabber-mam-last-id last-id)
      ;; Result set is complete
      (setq jabber-mam-done t))
    ;; Release lock
    (setq jabber-mam-lock t)
    nil))

(add-to-list 'jabber-message-chain 'jabber-handle-incoming-mam-message)
(defun jabber-handle-incoming-mam-message (jc xml-data)
  "Manage results from MAM request with connection JC and content XML-DATA.
The server returns message objects for each message using XMPP
Result Set Management.  Paging through results is performed in the
`jabber-mam-query' function.  Results are store in `jabber-mam-results'."
  (cond ((jabber-xml-get-children xml-data 'result)
         (let ((mam-jid-me (jabber-jid-user (jabber-xml-get-attribute
                                             xml-data 'to)))
               (mam-result (car (jabber-xml-get-children xml-data 'result))))
           (when (jabber-xml-get-children mam-result 'forwarded)
             ;; Extract message information (direction, timestamp, body), push
             ;; to results list
             (jabber-mam-process-entry mam-result))))
        ((jabber-xml-get-children xml-data 'fin)
         ;; End of set, determine if a subsequent query is required (if the
         ;; result is not complete).
         ;; Extract "complete" attribute from <fin> tag and <last> id
         (jabber-mam-process-fin xml-data))
        (t nil)))

(defun jabber-mam-report-success (jc xml-data context)
  "IQ callback reporting success or failure of the operation.
CONTEXT is a string describing the action.
\"CONTEXT succeeded\" or \"CONTEXT failed: REASON\" is displayed in
the echo area."
  (let ((type (jabber-xml-get-attribute xml-data 'type)))
    (message
     (concat context
             (if (string= type "result")
                 " succeeded"
		       (concat
                " failed: "
                (let ((the-error (jabber-iq-error xml-data)))
                  (if the-error
                      (jabber-parse-error the-error)
                    "No error message given"))))))
    (when (not (string= type "result"))
      (setq jabber-mam-done t
            jabber-mam-lock t))))

(defun jabber-mam-query (jc jid-me jid-with start-time end-time number
                            direction)
  "Build and send MAM query to server.
JC is jabber connection.  Messages between users with JIDs JID-ME JID and
JID-WITH JID with timestamp between START-TIME and END-TIME are retrieved.  The
set of results is limited to NUMBER messages.  DIRECTION is either \"in\" or
\"out\", or t for no limit on direction (this parameter is currently ignored)."
  ;; Initialize output and lock
  (setq jabber-mam-results (list))
  (setq jabber-mam-done nil)
  (setq jabber-mam-last-id nil)
  (let ((number-left (if (integerp number) number nil)))
    (while (not jabber-mam-done)
      (let ((mam-query (jabber-mam-make-query
                        jid-me jid-with
                        start-time
                        end-time
                        number-left
                        jabber-mam-last-id)))
        ;;(message "MAM request: [%s]" (jabber-sexp2xml mam-query))
        (setq jabber-mam-lock nil)
        (jabber-send-iq jc nil "set" mam-query
                        #'jabber-mam-report-success "MAM request"
                        #'jabber-mam-report-success "MAM request")
        ;; Wait for results
        (while (not jabber-mam-lock)
          (sit-for 1))
        ;; Update counter for remaining messages
        (when (integerp number)
          (setq number-left (- number (length jabber-mam-results)))
          (setq jabber-mam-done (or jabber-mam-done
                                    (<= number-left 0)))))))
  ;;(message "MAM got %d messages" (length jabber-mam-results))
  (nreverse jabber-mam-results))

(provide 'jabber-mam)
;;; jabber-mam.el ends here
