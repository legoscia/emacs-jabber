This is jabber.el 0.8.92, a Jabber client for Emacs.  Jabber (also known
as XMPP) is an instant messaging system; see http://www.jabber.org for
more information.

Home page:    http://emacs-jabber.sourceforge.net
Project page: http://sourceforge.net/projects/emacs-jabber
Wiki page:    http://www.emacswiki.org/cgi-bin/wiki/JabberEl
Mailing list: http://lists.sourceforge.net/lists/listinfo/emacs-jabber-general
and:          http://dir.gmane.org/gmane.emacs.jabber.general
MUC room:     jabber.el@conference.jabber.se and emacs@conference.jabber.ru (Russian, English)

GNU Emacs
=========

jabber.el runs on GNU Emacs 23.1 or later.

The file hexrgb.el (http://www.emacswiki.org/emacs/hexrgb.el) is
needed for MUC nick coloring feature.  A copy is located in the compat
directory, and used if the configure script doesn't find another copy
already installed.

XEmacs
======

You need an XEmacs with Mule support, and recent versions of the gnus,
net-utils and mule-ucs packages.  jabber.el basically works on XEmacs,
but some features are missing (in particular mouse support).  Testing
and patches are very welcome.

Encrypted connections
=====================
Many Jabber servers require encrypted connections, and even if yours
doesn't it may be good idea.  To get an encrypted connection, the most
convenient option is to use GNU Emacs 24 with GnuTLS support compiled
in.  You can check whether you have that by typing:

M-: (gnutls-available-p)

If that commands shows `t' in the echo area, then you have working
GnuTLS support.  If it shows `nil' or signals an error, then you
don't.

Failing that, jabber.el will use the starttls.el library, which
requires that the GnuTLS command line tool "gnutls-cli" is installed.
In Debian-based distributions, "gnutls-cli" is in the "gnutls-bin"
package.

The above applies to STARTTLS connections, the most common way to
encrypt a Jabber connection and the only one specified in the
standards.  STARTTLS connections start out unencrypted, but switch to
encrypted after negotiation.  jabber.el also supports connections that
are encrypted from start.  For this it uses the tls.el library, which
requires either "gnutls-cli" or the OpenSSL command line tool
"openssl" to be installed.

To use the latter form of encryption, customize jabber-account-list.

Note that only the connection from you to the server is encrypted;
there is no guarantee of connections from your server to your
contacts' server being encrypted.

Installation
============
jabber.el can be installed using the commands:
./configure
make
make install

You can specify which emacs you want to use:
./configure EMACS=emacs-or-xemacs-21.4

You can also install jabber.el by hand.  Put all .el files somewhere
in your load-path, or have your load-path include the directory
they're in.  To install the Info documentation, copy jabber.info to
/usr/local/info and run "install-info /usr/local/info/jabber.info".

After installation by either method, add (load "jabber-autoloads") to
your .emacs file.  (If you got the code from GIT, you still need the
makefile to generate jabber-autoloads.el.)

If you are upgrading from 0.7-0.7.x, you need to update your
configuration.  See the section "Account settings" in the manual.

Special notes for GIT version
=============================
If you are running jabber.el from GIT, you need to generate the
jabber-autoloads.el file yourself.  The simplest way to do this is by
using the "./configure && make" process.

To generate the configure script, make sure that autoconf and automake
are installed and run "autoreconf -i".

Usage
=====

To connect to a Jabber server, type C-x C-j C-c (or equivalently M-x
jabber-connect-all) and enter your JID.  With prefix argument,
register a new account.  You can set your JID permanently with M-x
jabber-customize.

Your roster is displayed in a buffer called *-jabber-*.  To
disconnect, type C-x C-j C-d or M-x jabber-disconnect.

You may want to use the menu bar to execute Jabber commands.  To
enable the Jabber menu, type M-x jabber-menu.

For a less terse description, read the enclosed manual.

For bug reports, help requests and other feedback, use the trackers
and forums at the project page mentioned above.

Configuration
=============
All available configuration options are described in the manual.  This
section only serves to point out the most important ones.

To change how you are notified about incoming events, type M-x
customize-group RET jabber-alerts.

To activate logging of all chats, set jabber-history-enabled to t.  By
default, history will be saved in ~/.jabber_global_message_log; make
sure that this file has appropriate permissions.  Type M-x
customize-group RET jabber-history for more options.

By default, jabber.el will send a confirmation when messages sent to
you are delivered and displayed, and also send "contact is typing"
notifications.  To change this, type M-x customize-group RET
jabber-events, and set the three jabber-events-confirm-* variables to
nil.

File transfer
=============
This release of jabber.el contains support for file transfer.  You may
need to configure some variables to make it work; see the manual for
details.

XMPP URIs
=========
It is possible to make various web browsers pass links starting with
"xmpp:" to jabber.el.  In the ideal case, this works right after
running "make install".  Otherwise, see the manual, section "XMPP
URIs".
