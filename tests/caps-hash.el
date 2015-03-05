;; Test disco hash against examples in XEP-0115

(message "Let's go")
(condition-case e
    (require 'jabber-disco)
  (error
   (message "disco bad! %S" e)))
(message "more")
(condition-case e
    (require 'jabber-widget)
  (error
   (message "bad! %S" e)))
(message "done!")

(let ((query
       (with-temp-buffer
	 (insert "<query xmlns='http://jabber.org/protocol/disco#info'
         node='http://psi-im.org#q07IKJEyjvHSyhy//CH0CxmKi8w='>
    <identity xml:lang='en' category='client' name='Psi 0.11' type='pc'/>
    <identity xml:lang='el' category='client' name='Ψ 0.11' type='pc'/>
    <feature var='http://jabber.org/protocol/caps'/>
    <feature var='http://jabber.org/protocol/disco#info'/>
    <feature var='http://jabber.org/protocol/disco#items'/>
    <feature var='http://jabber.org/protocol/muc'/>
    <x xmlns='jabber:x:data' type='result'>
      <field var='FORM_TYPE' type='hidden'>
        <value>urn:xmpp:dataforms:softwareinfo</value>
      </field>
      <field var='ip_version'>
        <value>ipv4</value>
        <value>ipv6</value>
      </field>
      <field var='os'>
        <value>Mac</value>
      </field>
      <field var='os_version'>
        <value>10.5.1</value>
      </field>
      <field var='software'>
        <value>Psi</value>
      </field>
      <field var='software_version'>
        <value>0.11</value>
      </field>
    </x>
  </query>")
	 (car (xml-parse-region (point-min) (point-max))))))
  (message "parsed xml")
  (unless (equal "q07IKJEyjvHSyhy//CH0CxmKi8w="
		 (jabber-caps-ver-string query "sha-1"))
    (error "Incorrect caps hash")))
