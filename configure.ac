
# check for one particular file of the sources 
AC_INIT(ocaml-flac, 0.1.0, savonet-users@lists.sourceforge.net)

VERSION=$PACKAGE_VERSION
AC_MSG_RESULT([configuring $PACKAGE_STRING])

# Add prefix to compilation variables
# if passed
if test "x$prefix" != "xNONE"; then
        CFLAGS="$CFLAGS -I$prefix/include"
        LDFLAGS="$LDFLAGS -L$prefix/lib"
        CPPFLAGS="$CPPFLAGS -I$prefix/include"
fi

AC_CHECK_OCAML_COMPILERS()

AC_PROG_CC()
AC_CHECK_TOOL([AR],[ar],no)
AC_SUBST(AR)
PKG_PROG_PKG_CONFIG()
#PKG_CHECK_MODULES(flac, flac, , [AC_MSG_ERROR(flac not found.)])

# Include a config.h
AC_CONFIG_HEADERS([config.h:config.h.in])
# Include it
CFLAGS="$CFLAGS -I`pwd`/`dirname $0`"
# Check endianess
AC_C_BIGENDIAN(AC_DEFINE([BIGENDIAN], [1], [The target is big endian]),[])

# PKG_CHECK_MODULES loses when you need --libs-only-[lL]
if ! $PKG_CONFIG --exists flac; then
        AC_MSG_ERROR([flac not found])
fi
flac_CFLAGS=`$PKG_CONFIG --cflags flac`
AC_SUBST([flac_CFLAGS])
flac_LIBS=`$PKG_CONFIG --libs-only-l flac`
AC_SUBST([flac_LIBS])
flac_LDFLAGS=`$PKG_CONFIG --libs-only-L flac`
AC_SUBST([flac_LDFLAGS])

# Ogg variables
OCAMLOGG_REQUIRES="ogg"
OCAMLOGG_META="package \"ogg\" (
  requires = \"flac ogg\"
  version = \"$VERSION\"
  description = \"Ogg/flac OCaml encoding/decoding module\"
  archive(byte)= \"flac.cma\"
  archive(native)= \"flac.cmxa\"
)"
OCAMLOGG_FILES="ogg_flac_stubs.c ogg_flac.ml ogg_flac.mli ogg_demuxer_flac_decoder.mli ogg_demuxer_flac_decoder.ml"
OCAMLOGG_LIBS="ogg"
OCAMLOGG_INC=

AC_ARG_WITH([ogg-dir],AC_HELP_STRING([--with-ogg-dir=path],
        [use "path" as the location of ocaml-ogg (autodetected by default)]))
if test -z "$with_ogg_dir"; then
  AC_MSG_CHECKING(for ocaml-ogg)
  if ($OCAMLFIND query ogg > /dev/null 2>&1); then
        OCAMLOGG_INC=`$OCAMLFIND query ogg`
        AC_MSG_RESULT(ok)
  else
        OCAMLOGG_REQUIRES=
        OCAMLOGG_META=
        OCAMLOGG_FILES=
        OCAMLOGG_LIBS=
        AC_MSG_RESULT(not found.)
  fi
else
  echo $with_ogg_dir | grep ^/ > /dev/null 2>&1 \
  || with_ogg_dir=$PWD/$with_ogg_dir
  OCAMLOGG_INC="$with_ogg_dir"
fi
AC_SUBST(OCAMLOGG_INC)
AC_SUBST(OCAMLOGG_FILES)
AC_SUBST(OCAMLOGG_LIBS)
AC_SUBST(OCAMLOGG_META)
AC_SUBST(OCAMLOGG_REQUIRES)

# substitutions to perform
AC_SUBST(VERSION)
AC_SUBST(INC)
AC_SUBST(requires)

# Finally create the Makefile and samples
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([src/META])
AC_CONFIG_FILES([src/Makefile])
AC_CONFIG_FILES([examples/Makefile.decode])
AC_CONFIG_FILES([examples/Makefile.encode])
AC_OUTPUT
