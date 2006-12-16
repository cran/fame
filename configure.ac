AC_INIT([fame], 0.99)
dnl See if user specified fame directory
hliDir=""
if test $FAME ; then
	hliDir="${FAME}/hli"
fi
AC_ARG_WITH([fameHliDir],
		    AC_HELP_STRING([--with-fameHliDir=DIR],
                           [specify the FAME HLI directory, home of hli.h and libchli.a]),
		    [hliDir=$withval])

if test [ -n "$hliDir" ] ; then
   AC_SUBST([CPPFLAGS], ["PKG_CPPFLAGS= -I${hliDir} -Dfame"])
   AC_SUBST([LIBS], ["PKG_LIBS= -L${hliDir} -lm -lc -lcrypt -lchli"])
else
   AC_MSG_NOTICE("Fame HLI directory not specified via FAME environment variable")
   AC_MSG_NOTICE("Fame HLI directory not specified via --with-fameHliDir argument")
   AC_MSG_WARN("No HLI directory was specified so FAME support will not be compiled. See the README file for help")
   AC_SUBST([CPPFLAGS], [""])
   AC_SUBST([LIBS], [""])
fi

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
