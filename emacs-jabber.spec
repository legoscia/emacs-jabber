# -*- coding: latin-1; mode: rpm-spec -*-

%define pkg_name jabber
%define cvsdate 20070428

Version: 0.8
Release: alt0.%cvsdate
Name: emacs-%pkg_name
Copyright: GPL
Group: Editors
Url: http://emacs-jabber.sourceforge.net
Summary: A minimal jabber client for Emacs

Packager: Emacs Maintainers Team <emacs@packages.altlinux.org>

Source: %name-%cvsdate.tar.gz
Source1: %pkg_name-emacs.el
Patch1: %pkg_name-roster.el-toggle-offline.patch

BuildArch: noarch
Requires: emacs-common emacs-gnus

BuildPreReq: emacs-devel >= 0.0.1-alt2

BuildPreReq: emacs-common
BuildPreReq: emacs-gnus

%description
jabber.el is a minimal Jabber client running under Emacs.

Recommends: xprintidle

%package el
Summary: The Emacs Lisp sources for bytecode included in %name
Group: Development/Other
Requires: %name = %version-%release

%description el
%name-el contains the Emacs Lisp sources for the bytecode
included in the %name package, that extends the Emacs editor.

You need to install %name-el only if you intend to modify any of the
%name code or see some Lisp examples.


%prep
%setup -qn %name
%patch1 -p0

%build
makeinfo jabber.texi

%install
%__mkdir_p %buildroot%_emacslispdir/%pkg_name
%__install -m 644 *.el %buildroot%_emacslispdir/%pkg_name/

%__mkdir_p %buildroot%_emacs_sitestart_dir
%__install -m 644 %SOURCE1 %buildroot%_emacs_sitestart_dir/%pkg_name.el

%__mkdir_p %buildroot%_infodir
%__install -m 644 %pkg_name.info %buildroot%_infodir/

%add_lisp_loadpath %buildroot%_emacslispdir/%pkg_name
%byte_recompile_lispdir

%__mkdir_p %buildroot%_bindir
%__install -m 755 xmppuri.sh %buildroot%_bindir

%post
%install_info %pkg_name.info

%preun
%uninstall_info %pkg_name.info


%files
%_bindir/*
%dir %_emacslispdir/%pkg_name
%_emacslispdir/%pkg_name/*.elc
%_infodir/%pkg_name.info*
%config(noreplace) %_emacs_sitestart_dir/%pkg_name.el
%doc README AUTHORS NEWS

%files el
%_emacslispdir/%pkg_name/*.el


%changelog
* Sat Apr 28 2007 Terechkov Evgenii <evg@altlinux.ru> 0.8-20070428
- Patch1 added

* Sat Apr 28 2007 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20070428
- cvs-20070428

* Thu Apr 19 2007 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20070419
- cvs-20070419
- All patches removed (commited in upstream)

* Mon Feb 19 2007 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20070219
- Patch0 added again
- Patch1 added (temporary)
- cvs-20070219

* Thu Feb  1 2007 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20070201
- cvs-20070201
- xmppuri.sh handler added

* Mon Jan  8 2007 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20070108
- Patch0 removed (fixed in upstream)

* Mon Jan  8 2007 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20070108
- cvs-20070108

* Thu Dec 14 2006 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20061214
- Patch0 replaced by new version

* Tue Dec 12 2006 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20061212
- Long actions patch added (#0)

* Fri Dec  8 2006 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20061208
- jabber-emacs.el additionals

* Sun Nov 19 2006 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20061119
- cvs-alt0.20061119

* Sun Oct 22 2006 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20061022
- cvs-20061022
- Obsolete patch really removed (author claims that he really fix bug)

* Sat Oct 21 2006 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20061021
- cvs-20061021
- Obsolete patch removed

* Fri Oct 13 2006 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20061013
- cvs-20061013

* Sat Aug 26 2006 Terechkov Evgenii <evg@altlinux.ru> 0.8-alt0.20060826
- cvs-20060826

* Thu Mar 02 2006 Eugene Vlasov <eugvv@altlinux.ru> 0.7-alt1
- Initial build for Sisyphus
