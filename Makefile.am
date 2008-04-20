my_lisp_sources=fsm.el jabber-activity.el jabber-ahc-presence.el	\
jabber-ahc.el jabber-alert.el jabber-autoaway.el jabber-avatar.el	\
jabber-bookmarks.el jabber-browse.el jabber-chat.el			\
jabber-chatbuffer.el jabber-chatstates.el jabber-compose.el		\
jabber-conn.el jabber-core.el jabber-disco.el jabber-events.el		\
jabber-export.el jabber-feature-neg.el jabber-festival.el		\
jabber-ft-client.el jabber-ft-common.el jabber-ft-server.el		\
jabber-gmail.el jabber-history.el jabber-iq.el jabber-keepalive.el	\
jabber-keymap.el jabber-logon.el jabber-menu.el jabber-modeline.el	\
jabber-muc-nick-completion.el jabber-muc.el jabber-newdisco.el		\
jabber-osd.el jabber-presence.el jabber-private.el jabber-ratpoison.el	\
jabber-register.el jabber-roster.el jabber-sasl.el jabber-sawfish.el	\
jabber-screen.el jabber-search.el jabber-si-client.el			\
jabber-si-common.el jabber-si-server.el jabber-socks5.el		\
jabber-time.el jabber-truncate.el jabber-util.el			\
jabber-vcard-avatars.el jabber-vcard.el jabber-version.el		\
jabber-watch.el jabber-widget.el jabber-wmii.el jabber-xmessage.el	\
jabber-xml.el jabber.el srv.el

dist_lisp_LISP=$(my_lisp_sources) jabber-autoload.el
MAINTAINERCLEANFILES=jabber-autoload.el

# The autoload file will cause Lisp sources to be rebuilt _twice_: the
# timestamp of the Lisp compilation is set _before_ the autoloads are
# regenerated, which means that jabber-autoload.el is once again
# considered new, which means that everything will be built again.
# Suggestions welcome.
jabber-autoload.el: $(my_lisp_sources)
	$(EMACS) --batch --eval "(setq generated-autoload-file \"$(abs_builddir)/$@\")" -f batch-update-autoloads $(srcdir)

info_TEXINFOS=jabber.texi

dist_libexec_SCRIPTS = xmppuri.sh

SUBDIRS = . tests

