AC_INIT(ollvm,0.99)

AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE([enable])

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

MAJOR=`echo "$OCAMLVERSION" | cut -d. -f1`
MINOR=`echo "$OCAMLVERSION" | cut -d. -f2`
if test "$MAJOR" -lt 4 || (test "$MAJOR" -eq 4 && test "$MINOR" -lt 01) ; then
  AC_MSG_ERROR([ocaml 4.01 or higher is required.])
fi

if test "$MAJOR" -lt 4 || (test "$MAJOR" -eq 4 && test "$MINOR" -lt 02) ; then

cat <<EOF > src/ollvm/ollvm.mli
module Ast : module type of Ollvm_ast
module Lexer : module type of Ollvm_lexer
module Printer : module type of Ollvm_printer
module Ez : module type of Ollvm_ez
EOF

else 

cat <<EOF > src/ollvm/ollvm.mli
module Ast = Ollvm_ast
module Lexer = Ollvm_lexer
module Printer = Ollvm_printer
module Ez = Ollvm_ez
EOF

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
