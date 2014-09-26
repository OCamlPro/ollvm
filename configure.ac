AC_INIT(ollvm,0.9.0)
AM_INIT_AUTOMAKE

AC_CONFIG_MACRO_DIR([m4])
AC_PROG_CC

# caml compilers and ocamldep
AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi
if test "$OCAMLOPT" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi
if test "$OCAMLDEP" = "no"; then
  AC_MSG_ERROR([ocamldep is required.])
fi
if test "$OCAMLDOC" = "no"; then
  AC_MSG_ERROR([ocamldoc is required.])
fi

# ocamlfind
AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([ocamlfind is required.])
fi

# ocamllex
AC_PROG_OCAMLLEX
if test "$OCAMLLEX" = "no"; then
  AC_MSG_ERROR([ocamllex is required.])
fi

# menhir
AC_CHECK_PROG(MENHIR, menhir, menhir, no)
if test "$MENHIR" = "no"; then
  AC_MSG_ERROR([menhir is required.])
fi

# llvm ocaml bindings
AC_CHECK_OCAML_PKG([llvm])
AM_CONDITIONAL([BUILD_LLVMGATEWAY], [test "$OCAML_PKG_llvm" != "no"])
if test "$OCAML_PKG_llvm" = "no" ; then
  AC_MSG_WARN([Llvmgateway module will not be installed.])
fi

AC_CONFIG_FILES([Makefile src/Makefile.common])
AC_OUTPUT
