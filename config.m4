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

  FFI_LIB_NAME=libffi

  AC_MSG_CHECKING(for libffi library in default path)
  for i in $PHP_FFI/lib /usr/$PHP_LIBDIR /usr/lib /usr/lib64 /usr/lib/x86_64-linux-gnu; do
    if test -r $i/${FFI_LIB_NAME}.${SHLIB_SUFFIX_NAME} -o -r $i/${FFI_LIB_NAME}.a; then
      FFI_LIB_DIR=$i
      AC_MSG_RESULT(found in $i)
      break
    fi
  done

  if test -z "$FFI_LIB_DIR"; then
    AC_MSG_RESULT(not found)
    AC_MSG_ERROR(Please reinstall the libffi distribution)
  fi

  AC_CHECK_TYPES(long double)

  AX_CHECK_COMPILE_FLAG([-std=c11], [ CFLAGS="$CFLAGS -std=c11"], AC_MSG_ERROR(Building FFI requires _Alignof(c11) supports))

  PHP_CHECK_LIBRARY(ffi, ffi_call, 
  [
	  PHP_CHECK_LIBRARY(ffi, ffi_prep_cif_var,
	  [
	    PHP_ADD_INCLUDE($FFI_DIR/include)
	    PHP_ADD_LIBRARY_WITH_PATH(ffi, $FFI_LIB_DIR, FFI_SHARED_LIBADD)
		AC_DEFINE(HAVE_FFI, 1, [ Have ffi support ])
	  ], [
		AC_MSG_ERROR(FFI module requires libffi above 3.0.11)
	  ], [
		-L$FFI_LIB_DIR
	  ])
  ], [
    AC_MSG_ERROR(FFI module requires libffi)
  ], [
	-L$FFI_LIB_DIR
  ])

  PHP_NEW_EXTENSION(ffi, ffi.c ffi_parser.c, $ext_shared)
  PHP_SUBST(FFI_SHARED_LIBADD)
fi
