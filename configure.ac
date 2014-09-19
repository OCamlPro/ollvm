AC_INIT(ollvm,0.1.0)
AM_INIT_AUTOMAKE

AC_CONFIG_MACRO_DIR([m4])
AC_PROG_CC

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([ocamlfind is required.])
fi

AC_PROG_OCAMLLEX
if test "$OCAMLLEX" = "no"; then
  AC_MSG_ERROR([ocamllex is required.])
fi

AC_CHECK_PROG(OCPBUILD, ocp-build, ocp-build, no)
if test "$OCPBUILD" = "no"; then
  AC_MSG_ERROR([ocp-build is required.])
fi

AC_CHECK_PROG(MENHIR, menhir, menhir, no)
if test "$MENHIR" = "no"; then
  AC_MSG_ERROR([menhir is required.])
fi

AC_CHECK_OCAML_PKG([llvm])
AM_CONDITIONAL([BUILD_LLVMGATEWAY], [test "$OCAML_PKG_llvm" = "no"])
if test ! "$BUILD_LLVMGATEWAY" ; then
  AC_MSG_WARN([Llvmgateway module will not be installed.])
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
