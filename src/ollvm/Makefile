LIBNAME=ollvm

SOURCES = \
	$(LIBNAME)_ast.ml \
	$(LIBNAME)_parser.mly \
	$(LIBNAME)_lexer.mll \
	$(LIBNAME)_printer.ml \
	$(LIBNAME)_ez.ml \
	$(LIBNAME).ml

CMI = \
	$(LIBNAME).cmi \
	$(LIBNAME)_ast.cmi \
	$(LIBNAME)_ez.cmi \
	$(LIBNAME)_printer.cmi \
	$(LIBNAME)_lexer.cmi

include ../Makefile.common
include .depend

$(BYTE): $(CMO)
	$(CAMLC) -a -o $(LIBNAME).cma $^

$(NATIVE): $(CMX)
	$(CAMLOPT) -a -o $(LIBNAME).cmxa $^
