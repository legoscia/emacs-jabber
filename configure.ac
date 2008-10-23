AC_INIT([jabber.el], [0.8.0], [emacs-jabber-general@lists.sourceforge.net], [emacs-jabber])
AM_INIT_AUTOMAKE([-Wall -Werror foreign dist-bzip2 dist-lzma dist-zip 1.9])
AC_CONFIG_MACRO_DIR([m4])

AM_PATH_LISPDIR
AS_IF([test "$EMACS" = no], [AC_MSG_ERROR([cannot find Emacs])])

AX_CHECK_EMACS_LIB([sha1], [HAVE_SHA1=yes], [HAVE_SHA1=no])
AX_CHECK_EMACS_LIB([sha1-el], [HAVE_SHA1_EL=yes], [HAVE_SHA1_EL=no])
AX_CHECK_EMACS_LIB([hex-util], [HAVE_HEX_UTIL=yes], [HAVE_HEX_UTIL=no])

AM_CONDITIONAL([USE_OUR_SHA1], [test x$HAVE_SHA1 = xno -a x$HAVE_SHA1_EL = xno])
AM_CONDITIONAL([USE_OUR_HEX_UTIL], [test x$HAVE_HEX_UTIL = xno])

dnl GConf schemas, for registering our URL handler
AC_PATH_PROG(GCONFTOOL, gconftool-2, no)
AM_CONDITIONAL(USE_GCONFTOOL, test "x$GCONFTOOL" != "xno")
AM_GCONF_SOURCE_2

AC_CONFIG_FILES([Makefile tests/Makefile gconf/Makefile jabber-ourversion.el])
AC_OUTPUT
