(require 'jabber-iq)
(require 'jabber-xml)

(defun jabber-carbon-success (jc xml-data context)
  (when (equal "result" (jabber-xml-get-attribute xml-data 'type))
    (message "Carbons feature successfully enabled")))

(defun jabber-carbon-failure (jc xml-data context)
  (message "Carbons feature could not be enabled: %S" xml-data))

(add-to-list 'jabber-jid-service-menu
             (cons "Enable Carbons" 'jabber-enable-carbons))
(defun jabber-enable-carbons (jc)
  "Send request to enable XEP-0280 Message Carbons"
  (interactive (list (jabber-read-account)))
  (jabber-send-iq jc
                  nil
                  "set"
                  `(enable ((xmlns . "urn:xmpp:carbons:2")))
                  #'jabber-carbon-success "Carbons feature enablement"
                  #'jabber-carbon-failure "Carbons feature enablement"))

(provide 'jabber-carbons)
