dnl $Id$
dnl config.m4 for extension FFI

PHP_ARG_WITH(ffi, for FFI support,
[  --with-ffi             Include FFI support])

if test "$PHP_FFI" != "no"; then
  if test -r $PHP_FFI/include/ffi.h; then
    FFI_DIR=$PHP_FFI
  else
    AC_MSG_CHECKING(for libffi in default path)
    for i in /usr/local /usr; do
      if test -r $i/include/ffi.h; then
        FFI_DIR=$i
        AC_MSG_RESULT(found in $i)
        break
      fi
    done
  fi

  if test -z "$FFI_DIR"; then
    AC_MSG_RESULT(not found)
    AC_MSG_ERROR(Please reinstall the libffi distribution)
  fi

  PHP_CHECK_LIBRARY(ffi, ffi_call, 
  [
    PHP_ADD_INCLUDE($FFI_DIR/include)
    PHP_ADD_LIBRARY_WITH_PATH(ffi, $FFI_DIR/$PHP_LIBDIR, FFI_SHARED_LIBADD)
    AC_DEFINE(HAVE_FFI,1,[ Have ffi support ])
  ], [
    AC_MSG_ERROR(FFI module requires libffi)
  ], [
    -L$FFI_DIR/$PHP_LIBDIR
  ])

  PHP_NEW_EXTENSION(ffi, ffi.c ffi_parser.c, $ext_shared)
  PHP_SUBST(FFI_SHARED_LIBADD)
fi
