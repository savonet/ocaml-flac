
# check for one particular file of the sources 
AC_INIT([ocaml-flac],[0.1.4],[savonet-users@lists.sourceforge.net])

VERSION=$PACKAGE_VERSION
AC_MSG_RESULT([configuring $PACKAGE_STRING])

AC_BASE_CHECKS()

PKG_PROG_PKG_CONFIG()
PKG_CONFIG_CHECK_MODULE([flac])
#PKG_CHECK_MODULES(flac, flac, , [AC_MSG_ERROR(flac not found.)])

# Include a config.h
AC_CONFIG_HEADERS([config.h:config.h.in])
# Include it
CFLAGS="$CFLAGS -I .."
# Check endianess
AC_C_BIGENDIAN(AC_DEFINE([BIGENDIAN], [1], [The target is big endian]),[])

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

AC_ARG_WITH([ogg-dir],AS_HELP_STRING([--with-ogg-dir=path],[use "path" as the location of ocaml-ogg (autodetected by default)]))
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
        AC_MSG_ERROR(not found.)
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

# Bytes.
AC_CHECK_OCAML_BINDING([bytes],[],[],[1])
INC="$INC `$OCAMLFIND query bytes`"

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
