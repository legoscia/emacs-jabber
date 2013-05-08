AC_INIT([jabber.el], [0.8.92], [emacs-jabber-general@lists.sourceforge.net], [emacs-jabber])
AC_CONFIG_AUX_DIR([build-aux])
dnl Need automake 1.11 for dist-xz option
AM_INIT_AUTOMAKE([1.11 -Wall -Werror foreign dist-bzip2 dist-xz dist-zip])
AC_CONFIG_MACRO_DIR([m4])

AM_PATH_LISPDIR
AS_IF([test "$EMACS" = no], [AC_MSG_ERROR([cannot find Emacs])])

dnl GConf schemas, for registering our URL handler
AC_PATH_PROG(GCONFTOOL, gconftool-2, no)
AM_CONDITIONAL(USE_GCONFTOOL, test "x$GCONFTOOL" != "xno")
AM_CONDITIONAL(GCONF_SCHEMAS_INSTALL, false)
m4_ifndef([AM_GCONF_SOURCE_2], [m4_defun([AM_GCONF_SOURCE_2])])
AM_GCONF_SOURCE_2

AC_CONFIG_FILES([Makefile tests/Makefile gconf/Makefile jabber-ourversion.el])
AC_OUTPUT
