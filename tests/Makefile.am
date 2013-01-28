# LOG_COMPILER was introduced in Automake 1.12; don't expect "make
# check" or "make distcheck" to work with earlier versions.
LOG_COMPILER = env top_builddir=$(top_builddir) $(EMACS) -batch -L $(top_builddir) -L $(top_srcdir) -L $(srcdir) -l
TESTS = load-all.el skip-tag-forward.el history.el jabberd.el nick-change-fail.el
dist_noinst_DATA = $(TESTS)
