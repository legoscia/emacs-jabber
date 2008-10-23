schemadir = $(GCONF_SCHEMA_FILE_DIR)

schema_DATA = emacs-jabber.schemas
CLEANFILES = $(schema_DATA)
EXTRA_DIST = emacs-jabber.schemas.in

emacs-jabber.schemas: emacs-jabber.schemas.in Makefile
	sed -e "s|@""libexecdir@""|$(libexecdir)|" < $(srcdir)/emacs-jabber.schemas.in > emacs-jabber.schemas

install-data-local:
	GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $(schema_DATA)

uninstall-local:
	GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-uninstall-rule $(schema_DATA)
