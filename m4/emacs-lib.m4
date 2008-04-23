# AX_EMACS_RUN_IFELSE(PROGRAM, ACTION-IF-TRUE, ACTION-IF-FALSE)
# -------------------------------------------------------------
# Run PROGRAM in emacs.  If it finishes successfully, execute
# ACTION-IF-TRUE, else ACTION-IF-FALSE.
AC_DEFUN([AX_EMACS_RUN_IFELSE],
[cat >conftest.el <<EOF
$1
EOF
AC_RUN_LOG([$EMACS -batch -l conftest.el])
AS_IF([test $ac_status -eq 0], [$2], [$3])])

# AX_CHECK_EMACS_LIB(LIBRARY, ACTION-IF-PRESENT, ACTION-IF-NOT)
# -------------------------------------------------------------
# Check whether emacs can load LIBRARY with require.  Execute
# ACTION-IF-PRESENT if it can, else ACTION-IF-NOT.
AC_DEFUN([AX_CHECK_EMACS_LIB],
[
AC_CACHE_CHECK([whether $EMACS has library $1],
	       [AS_TR_SH([ax_cv_emacs_lib_$1])],
	       [AX_EMACS_RUN_IFELSE([(require '$1)],
	       		            [AS_TR_SH([ax_cv_emacs_lib_$1])=yes],
				    [AS_TR_SH([ax_cv_emacs_lib_$1])=no])])
AS_IF([test $AS_TR_SH([ax_cv_emacs_lib_$1]) = yes], [$2], [$3])
])
