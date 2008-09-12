AC_INIT([fame], 2.3)
dnl initialize to blanks
AC_SUBST([CPPFLAGS], [""])
AC_SUBST([LIBS], [""])
fameDir=""
includeDir=""
libType="none"

dnl See if user specified fame directory
if test $FAME ; then
	fameDir="${FAME}"
fi
AC_ARG_WITH([fameDir],
		    AC_HELP_STRING([--with-fameDir=DIR],
                           [specify the FAME directory, which may have an hli subdirectory]),
		    [fameDir=$withval])

if test $fameDir; then
  AC_CHECK_FILE("$fameDir/hli.h",      includeDir="$fameDir")
  AC_CHECK_FILE("$fameDir/hli/hli.h",  includeDir="$fameDir/hli")

  dnl find out if this is 64-bit version of R
  libnn=`R CMD config LIBnn`

  dnl set libDir to directory containing chli.dll, libchli.so, or libchli.a
  if test "$libnn" = "lib64" ; then
    libDir="$fameDir/hli/64"
  else
    libDir="$fameDir/hli"
  fi
  AC_CHECK_FILE("$libDir/hli", libDir="$libDir/hli")
  AC_CHECK_FILE("$libDir/libchli.a",  libType="static")
  AC_CHECK_FILE("$libDir/libchli.so", libType="dynamic")
fi

if test "$libType" = "none" -o "$includeDir" = "" ; then
  AC_MSG_NOTICE("HLI interface will not built because")
  if test "$fameDir" = "" ; then
    AC_MSG_NOTICE("No fameDir specified via --with-fameDir or FAME environment variable.")
  fi
  if test "$includeDir" = "" ; then
    AC_MSG_NOTICE("Fame HLI header hli.h not found")
  fi
  if test "$libType" = "none" ; then
    AC_MSG_NOTICE("Fame HLI library (libchli.so or libchli.a) not found.")
  fi
else
  AC_SUBST([CPPFLAGS], ["PKG_CPPFLAGS= -I${includeDir} -Dfame"])
  if test "$libType" = "static" ; then
    AC_SUBST([LIBS], ["PKG_LIBS= -L${libDir} -lm -lc -lcrypt -lchli"])
  fi
  if test "$libType" = "dynamic" ; then
    AC_SUBST([LIBS], ["PKG_LIBS= -lm -lc -lcrypt -L${libDir} -Wl,-rpath=${libDir} -lchli"])
  fi
fi				  

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
