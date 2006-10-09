;;; fsm.el --- state machine library

;; Copyright (C) 2006  Magnus Henoch

;; Author: Magnus Henoch <mange@freemail.hu>
;; Version: 0.1

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

;; fsm.el is an exercise in metaprogramming inspired by gen_fsm of
;; Erlang/OTP.  It aims to make asynchronous programming in Emacs Lisp
;; easy and fun.  By "asynchronous" I mean that long-lasting tasks
;; don't interfer with normal editing.

;; Some people say that it would be nice if Emacs Lisp had threads
;; and/or continuations.  They are probably right, but there are few
;; things that can't be made to run in the background using facilities
;; already available: timers, filters and sentinels.  As the code can
;; become a bit messy when using such means, with callbacks everywhere
;; and such things, it can be useful to structure the program as a
;; state machine.

;; In this model, a state machine passes between different "states",
;; which are actually only different event handler functions.  The
;; state machine receives "events" (from timers, filters, user
;; requests, etc) and reacts to them, possibly entering another state,
;; possibly returning a value.

;; The essential macros/functions are:
;; 
;; define-state-machine  - create start-FOO function
;; define-state          - event handler for each state (required)
;; define-enter-state    - called when entering a state (optional)
;; fsm-send              - send an event to a state machine
;; fsm-call              - send an event and wait for reply

;; fsm.el is similar to but different from Distel:
;; <URL:http://fresh.homeunix.net/~luke/distel/>
;; Emacs' tq library is a similar idea.

;;; Code:

(eval-when-compile (require 'cl))

(defvar fsm-debug "*fsm-debug*"
  "Name of buffer for fsm debug messages.
If nil, don't output debug messages.")

(defun fsm-debug-output (format &rest args)
  "Append debug output to buffer named by `fsm-debug'.
FORMAT and ARGS are passed to `format'."
  (when fsm-debug
    (with-current-buffer (get-buffer-create fsm-debug)
      (save-excursion
	(goto-char (point-max))
	(insert (current-time-string) ": " (apply 'format format args) "\n")))))

(defmacro* define-state-machine (name &key start sleep)
  "Define a state machine class called NAME.
A function called start-NAME is created, which uses the argument
list and body specified in the :start argument.  BODY should
return a list of the form (STATE STATE-DATA [TIMEOUT]), where
STATE is the initial state (defined by `define-state'),
STATE-DATA is any object, and TIMEOUT is the number of seconds
before a :timeout event will be sent to the state machine.  BODY
may refer to the instance being created through the dynamically
bound variable `fsm'.

SLEEP-FUNCTION, if provided, takes one argument, the number of
seconds to sleep while allowing events concerning this state
machine to happen.  There is probably no reason to change the
default, which is accept-process-output with rearranged
arguments.

\(fn NAME :start ((ARG ...) DOCSTRING BODY) [:sleep SLEEP-FUNCTION])"
  (let ((start-name (intern (concat "start-" (symbol-name name)))))
    (destructuring-bind (arglist docstring &body body) start
      (unless (stringp docstring)
	(error "Docstring is not a string"))
      `(progn
	 (defun ,start-name ,arglist
	   ,docstring
	   (fsm-debug-output "Starting %s" ',name)
	   (let ((fsm (list :fsm ',name)))
	     (destructuring-bind (state state-data &optional timeout)
		 (progn ,@body)
	       (nconc fsm (list :state nil :state-data nil
				:sleep ,(or sleep (lambda (secs) (accept-process-output nil secs)))
				:deferred nil))
	       (fsm-update fsm state state-data timeout)
	       fsm)))))))

(defmacro* define-state (fsm state-name arglist &body body)
  "Define a state called STATE-NAME in the state machine FSM.
ARGLIST and BODY make a function that gets called when the state
machine receives an event in this state.  The arguments are:

FSM         the state machine instance (treat it as opaque)
STATE-DATA  An object
EVENT       The occurred event, an object.
CALLBACK    A function of one argument that expects the response
            to this event, if any (often `ignore' is used)

If the event should return a response, the state machine should
arrange to call CALLBACK at some point in the future (not necessarily
in this handler).

The function should return a list of the form (NEW-STATE
NEW-STATE-DATA TIMEOUT):

NEW-STATE      The next state, a symbol
NEW-STATE-DATA An object
TIMEOUT        A number: send timeout event after this many seconds
               nil: cancel existing timer
               :keep: let existing timer continue

Alternatively, the function may return the keyword :defer, in
which case the event will be resent when the state machine enters
another state."
  (let ((fn-name (intern (concat "fsm-" (symbol-name fsm) "-" (symbol-name state-name)))))
    `(defun ,fn-name ,arglist
       ,@body)))

(defmacro* define-enter-state (fsm state-name arglist &body body)
  "Define a function to call when FSM enters the state STATE-NAME.
ARGLIST and BODY make a function that gets called when the state
machine enters this state.  The arguments are:

FSM         the state machine instance (treat it as opaque)
STATE-DATA  An object

The function should return a list of the form (NEW-STATE-DATA
TIMEOUT):

NEW-STATE-DATA An object
TIMEOUT        A number: send timeout event after this many seconds
               nil: cancel existing timer
               :keep: let existing timer continue"
  (let ((fn-name (intern (concat "fsm-" (symbol-name fsm) "-enter-" (symbol-name state-name)))))
    `(defun ,fn-name ,arglist
       ,@body)))

(defun fsm-start-timer (fsm secs)
  "Send a timeout event to FSM after SECS seconds.
The timer is canceled if another event occurs before, unless the
event handler explicitly asks to keep the timer."
  (fsm-stop-timer fsm)
  (setf (cddr fsm) 
	(plist-put 
	 (cddr fsm) 
	 :timeout (run-with-timer secs
				  nil
				  #'fsm-send-sync fsm 
				  :timeout))))

(defun fsm-stop-timer (fsm)
  "Stop the timeout timer of FSM."
  (let ((timer (plist-get (cddr fsm) :timeout)))
    (when (timerp timer)
      (cancel-timer timer)
      (setf (cddr fsm) (plist-put (cddr fsm) :timeout nil)))))

(defun fsm-maybe-change-timer (fsm timeout)
  "Change the timer of FSM according to TIMEOUT."
  (cond
   ((numberp timeout)
    (fsm-start-timer fsm timeout))
   ((null timeout)
    (fsm-stop-timer fsm))
   ;; :keep needs no timer change
   ))

(defun fsm-send (fsm event &optional callback)
  "Send EVENT to FSM asynchronously.
If the state machine generates a response, eventually call
CALLBACK with the response as only argument."
  (run-with-timer 0.1 nil #'fsm-send-sync fsm event callback))

(defun fsm-update (fsm new-state new-state-data timeout)
  (let ((old-state (plist-get (cddr fsm) :state)))
    (plist-put (cddr fsm) :state new-state)
    (plist-put (cddr fsm) :state-data new-state-data)
    (fsm-maybe-change-timer fsm timeout)

    ;; On state change, call enter function and send deferred events
    ;; again.
    (unless (eq old-state new-state)
      (fsm-debug-output "%s enters %s" (cadr fsm) new-state)
      (let ((enter-fn (intern (concat "fsm-" (symbol-name (cadr fsm)) "-enter-" (symbol-name new-state)))))
	(when (functionp enter-fn)
	  (destructuring-bind (newer-state-data newer-timeout)
	      (funcall enter-fn fsm new-state-data)
	    (plist-put (cddr fsm) :state-data newer-state-data)
	    (fsm-maybe-change-timer fsm newer-timeout))))

      (let ((deferred (nreverse (plist-get (cddr fsm) :deferred))))
	(setf (cddr fsm)
	      (plist-put (cddr fsm) :deferred nil))
	(dolist (event deferred)
	  (apply 'fsm-send-sync fsm event))))))

(defun fsm-send-sync (fsm event &optional callback)
  "Send EVENT to FSM synchronously.
If the state machine generates a response, eventually call
CALLBACK with the response as only argument."
  (save-match-data
    (let* ((fsm-name (second fsm))
	   (state (plist-get (cddr fsm) :state))
	   (state-data (plist-get (cddr fsm) :state-data))
	   (state-fn (intern (concat "fsm-" (symbol-name fsm-name)
				     "-" (symbol-name state)))))
      ;; If the event is a list, output only the car, to avoid an
      ;; overflowing debug buffer.
      (fsm-debug-output "Sent %S to %s in state %s" (or (car-safe event) event) fsm-name state)
      (let ((result (condition-case e
			(funcall state-fn fsm state-data event (or callback 'ignore))
		      (error (cons :error-signaled e)))))
	;; Special case for deferring an event until next state change.
	(cond
	 ((eq result :defer)
	  (let ((deferred (plist-get (cddr fsm) :deferred)))
	    (plist-put (cddr fsm) :deferred (cons (list event callback) deferred))))
	 ((null result)
	  (fsm-debug-output "Warning: event %S ignored in state %s" event state))
	 ((eq (car-safe result) :error-signaled)
	  (fsm-debug-output "Error: %s" (error-message-string (cdr result))))
	 (t
	  (destructuring-bind (new-state new-state-data &optional timeout) result
	    (fsm-update fsm new-state new-state-data timeout))))))))

(defun fsm-call (fsm event)
  "Send EVENT to FSM synchronously, and wait for a reply.
Return the reply.
`with-timeout' might be useful."
  (lexical-let (reply)
    (fsm-send-sync fsm event (lambda (r) (setq reply (list r))))
    (while (null reply)
      (fsm-sleep fsm 1))
    (car reply)))

(defun fsm-make-filter (fsm)
  "Return a filter function that sends events to FSM.
Events sent are of the form (:filter PROCESS STRING)."
  (lexical-let ((fsm fsm))
    (lambda (process string)
      (fsm-send-sync fsm (list :filter process string)))))

(defun fsm-make-sentinel (fsm)
  "Return a sentinel function that sends events to FSM.
Events sent are of the form (:sentinel PROCESS STRING)."
  (lexical-let ((fsm fsm))
     (lambda (process string)
       (fsm-send-sync fsm (list :sentinel process string)))))

(defun fsm-sleep (fsm secs)
  "Sleep up to SECS seconds in a way that lets FSM receive events."
  (funcall (plist-get (cddr fsm) :sleep) secs))

(provide 'fsm)
;;; fsm.el ends here
