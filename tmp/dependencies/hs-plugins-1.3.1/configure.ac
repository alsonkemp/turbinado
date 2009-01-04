#
# Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
# LGPL version 2.1 or later (see http://www.gnu.org/copyleft/lesser.html)
#

# SUBSTs go into config.mk.in (TOP,PREFIX,GHC,INSTAlL)
# AC_DEFINES go into config.h
# only AC_DEFINES are visible in Haskell src

# sanity test
AC_INIT(src/System/Plugins.hs)

# Prepare to generate the config.h.in file
AC_CONFIG_HEADERS([config.h])

# untested on earlier than 2.52, but it won't work anyway
AC_PREREQ(2.53)

# Find out what type of system we're running on
AC_CANONICAL_BUILD

Platform="$build_cpu-$build_vendor-$build_os"

# This isn't right for msys mingw.
PREFIX="$prefix"
if test "$prefix" = "NONE"
then
  if test "$build_os" = "cygwin"
  then
    PREFIX="c:/cygwin$ac_default_prefix"
  else
    PREFIX="$ac_default_prefix"
  fi
fi
AC_SUBST(PREFIX)

case $Platform in
*-apple-darwin*)
        MACOSX=yes
        AC_DEFINE_UNQUOTED(MACOSX, [1], [Defined if compiling on the mac])
        LEADING_UNDERSCORE=1
	;;
*)
        MACOSX=no
        LEADING_UNDERSCORE=0
	;;
esac

if test "$build_os" = "mingw32"
then
  AC_DEFINE(__MINGW32__,[1],[Defined if compiling with mingw])
  LEADING_UNDERSCORE=1
fi

if test "$build_os" = "cygwin"
then
  AC_DEFINE(CYGWIN,[1],[Defined if compiling with mingw])
  LEADING_UNDERSCORE=1
fi

AC_DEFINE_UNQUOTED(LEADING_UNDERSCORE, $LEADING_UNDERSCORE, 
                [Whether symbols are prefixed with a leading underscore])

# Find pwd, in a cygwin friendly way. 
# Suggested by: http://www.haskell.org/ghc/docs/latest/html/users_guide/ch11s04.html
TOP=`echo "Directory.getCurrentDirectory >>= putStrLn.init.tail.show " | ghc --interactive -ignore-dot-ghci -v0 | tr -d '\r'`

AC_DEFINE_UNQUOTED(TOP, "$TOP", [Path to top of build tree])
AC_SUBST(TOP)

# necessary tools

# allow user supplied haskell compiler
AC_ARG_WITH(ghc,
  AC_HELP_STRING([--with-ghc=<ghc>],[use a specific Haskell compiler]),
  [ GHC="$withval" 
    if test ! -f "$GHC" ; then
       AC_MSG_ERROR([$GHC not found. You need GHC to build this project])
    fi
  ],
  [ AC_CHECK_PROG(GHC,ghc,ghc)
    if test -z "$GHC" ; then
       AC_MSG_ERROR([You need GHC to build this project])
    fi 
  ]
  )
AC_DEFINE_UNQUOTED(WITH_GHC, "$GHC", [Which ghc to use])
AC_SUBST(GHC)

# Work out value of __GLASGOW_HASKELL__
if test -n "$GHC" ; then
 AC_MSG_CHECKING([for value of __GLASGOW_HASKELL__])
 echo "main = print __GLASGOW_HASKELL__" > t.hs
 GLASGOW_HASKELL=`echo 'main' | "$GHC" --interactive -ignore-dot-ghci -v0 -cpp t.hs`
 rm t.hs
 AC_MSG_RESULT([$GLASGOW_HASKELL])
fi
AC_SUBST(GLASGOW_HASKELL)

# find path to GHC libs, for runtime_loader
if test -n "$GHC" ; then
    AC_MSG_CHECKING([for ghc library directory])
    GHC_LIB_PATH=`$GHC --print-libdir | tr -d '\r'`
    AC_MSG_RESULT([$GHC_LIB_PATH])
fi
AC_DEFINE_UNQUOTED(GHC_LIB_PATH, "$GHC_LIB_PATH", [Path to ghc libraries])

# Allow a debugging version of hs-plugins to be built
AC_ARG_ENABLE(debug,
   [  --enable-debug          Enable a debug version of hs-plugins to be built],
   [ 
     AC_DEFINE_UNQUOTED(DEBUG, [1], [Defined if a debugging version is to be built]) ],
   [ : ])

# Allow a debugging version of hs-plugins to be built
AC_ARG_ENABLE(hsx,
   [  --enable-hsx          Enable a debug version of hs-plugins to be built],
   [ HASKELL_SRC=haskell-src-exts ;
     AC_DEFINE_UNQUOTED(WITH_HSX, [1], 
          [Defined if hs-plugins is to be built with Niklas Broberg's HSX parser]) ],
   [ HASKELL_SRC=haskell-src ])

AC_SUBST(HASKELL_SRC)

#AC_CHECK_PROG(TEX,tex,tex)
#if test -z "$TEX" ; then
#    AC_MSG_WARN(tex is needed to build some of the documentation)
#fi
#AC_CHECK_PROG(TEX2PAGE,tex2page,tex2page)
#if test -z "$TEX2PAGE" ; then
#    AC_MSG_WARN(tex2page is needed to build some of the documentation
#		    http://www.ccs.neu.edu/home/dorai/tex2page/tex2page-doc.html)
#fi

AC_CHECK_FUNCS(arc4random)

AC_PROG_INSTALL

# System.Process uses cmd or command.com shell on Windows, doen't use Cygwin or MSYS shell
AC_CONFIG_FILES([config.mk testsuite/makewith/io/TestIO.conf testsuite/makewith/unsafeio/Unsafe.conf])
AC_OUTPUT

