AC_INIT([fame], 1.08)
dnl See if user specified fame directory
hliDir=""
if test $FAME ; then
	hliDir="${FAME}/hli"
fi
AC_ARG_WITH([fameHliDir],
		    AC_HELP_STRING([--with-fameHliDir=DIR],
                           [specify the FAME HLI directory, home of hli.h and libchli.a]),
		    [hliDir=$withval])

hliType="none"
if test [ -n "$hliDir" ] ; then
   AC_CHECK_FILE("$hliDir",            hliType="nodir")
   AC_CHECK_FILE("$hliDir/libchli.a",  hliType="static")
   AC_CHECK_FILE("$hliDir/libchli.so", hliType="dynamic")
fi

AC_SUBST([CPPFLAGS], [""])
AC_SUBST([LIBS], [""])
AC_SUBST([HAVE_FAME], ["FALSE"])

if test "$hliType" = "none" ; then
   AC_MSG_NOTICE("Fame HLI directory not specified via FAME environment variable")
   AC_MSG_NOTICE("Fame HLI directory not specified via --with-fameHliDir argument")
   AC_MSG_WARN("No HLI directory was specified so FAME support will not be compiled. See the README file for help")
fi

if test "$hliType" = "nodir" ; then
   AC_MSG_WARN("Specified HLI directory does not exist. FAME support will not be compiled. See the README file for help")
fi

if test "$hliType" = "static" ; then
   AC_SUBST([CPPFLAGS], ["PKG_CPPFLAGS= -I${hliDir} -Dfame"])
   AC_SUBST([LIBS], ["PKG_LIBS= -L${hliDir} -lm -lc -lcrypt -lchli"])
   AC_SUBST([HAVE_FAME], ["TRUE"])
fi

if test "$hliType" = "dynamic" ; then
   AC_SUBST([CPPFLAGS], ["PKG_CPPFLAGS= -I${hliDir} -Dfame"])
   AC_SUBST([LIBS], ["PKG_LIBS= -lm -lc -lcrypt -L${hliDir} -Wl,-rpath=${hliDir} -lchli"])
   AC_SUBST([HAVE_FAME], ["TRUE"])
fi

AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_FILES([R/unix/zzz.R])
AC_OUTPUT
