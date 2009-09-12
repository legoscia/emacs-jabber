ACLOCAL_AMFLAGS = -I m4
# The following line needs to be here since automake 1.4 doesn't look
# for options in configure.ac.
AUTOMAKE_OPTIONS = 1.9

my_lisp_sources=fsm.el jabber-activity.el jabber-ahc-presence.el	\
jabber-ahc.el jabber-alert.el jabber-autoaway.el jabber-avatar.el	\
jabber-awesome.el							\
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

compat_lisp_sources =
if USE_OUR_SHA1
compat_lisp_sources += compat/sha1.el
endif
if USE_OUR_HEX_UTIL
compat_lisp_sources += compat/hex-util.el
endif
# automake gets confused and doesn't clean up these two compiled files.
CLEANFILES=sha1.elc hex-util.elc

dist_lisp_LISP=$(my_lisp_sources) $(compat_lisp_sources) jabber-autoloads.el jabber-ourversion.el
MAINTAINERCLEANFILES=jabber-autoloads.el

EXTRA_DIST = jabber-pkg.el.in

# The autoload file will cause Lisp sources to be rebuilt _twice_: the
# timestamp of the Lisp compilation is set _before_ the autoloads are
# regenerated, which means that jabber-autoload.el is once again
# considered new, which means that everything will be built again.
# Suggestions welcome.
#
# Emacs 21 requires that the file already exist and have non-zero
# size, so we touch it with ancient timestamp.
jabber-autoloads.el: $(my_lisp_sources)
	test -s $@ || cp -f $(srcdir)/jabber-autoloads.stub $@; touch -t 197001030000 $@
	$(EMACS) --batch --eval "(setq generated-autoload-file \"$(abs_builddir)/$@\")" -f batch-update-autoloads $(srcdir)

info_TEXINFOS=jabber.texi

dist_libexec_SCRIPTS = emacs-jabber-uri-handler

SUBDIRS = . tests
if USE_GCONFTOOL
if GCONF_SCHEMAS_INSTALL
SUBDIRS += gconf
endif
endif

# Package everything in a form suitable for ELPA.  That is, use
# "jabber" instead of "emacs-jabber" as base name.
CLEANFILES += jabber-pkg.el
elpa: dist
	rm -rf emacs-jabber-$(PACKAGE_VERSION) jabber-$(PACKAGE_VERSION)
	$(AMTAR) xzf emacs-jabber-$(PACKAGE_VERSION).tar.gz
	mv emacs-jabber-$(PACKAGE_VERSION) jabber-$(PACKAGE_VERSION)
	cd jabber-$(PACKAGE_VERSION) ; install-info jabber.info dir
	sed "s/@""PACKAGE_VERSION@""/$(PACKAGE_VERSION)/" < $(srcdir)/jabber-pkg.el.in > jabber-$(PACKAGE_VERSION)/jabber-pkg.el
	$(AMTAR) chf jabber-$(PACKAGE_VERSION).tar jabber-$(PACKAGE_VERSION)
	rm -rf jabber-$(PACKAGE_VERSION)
	@echo "Created jabber-$(PACKAGE_VERSION).tar"

