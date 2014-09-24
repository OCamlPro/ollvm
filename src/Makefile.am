#*********************************************************************#
#                                                                     #
#                           Objective Caml                            #
#                                                                     #
#            Pierre Weis, projet Cristal, INRIA Rocquencourt          #
#                                                                     #
# Copyright 1998, 2004 Institut National de Recherche en Informatique #
# et en Automatique. Distributed only by permission.                  #
#                                                                     #
#*********************************************************************#

CAMLC =$(OCAMLFIND) ocamlc
CAMLOPT =$(OCAMLFIND) ocamlopt
CAMLDEP =$(OCAMLDEP)
CAMLLEX =$(OCAMLLEX)
CAMLYACC =$(MENHIR)
CAMLDOC =$(OCAMLFIND) ocamldoc

LIBNAME=ollvm

SOURCES = ollvm_ast.ml ollvm_parser.mly ollvm_lexer.mll ollvm_printer.ml ollvm_ez.ml
CMILIB = $(LIBNAME).cmi ollvm_ast.cmi ollvm_ez.cmi ollvm_printer.cmi ollvm_lexer.cmi
CAMLFLAGS =

if BUILD_LLVMGATEWAY
  SOURCES += ollvm_llvmgateway.ml
  CMILIB += ollvm_llvmgateway.cmi
  CAMLFLAGS += -package llvm
endif

BYTELIB = $(LIBNAME).cma
NATIVELIB = $(LIBNAME).cmxa $(LIBNAME).a

SOURCES1 = $(SOURCES:.mly=.ml)
SOURCES2 = $(SOURCES1:.mll=.ml)
CMO = $(SOURCES2:.ml=.cmo)
CMX = $(SOURCES2:.ml=.cmx)

DOCDIR=doc

all: depend $(BYTELIB) $(NATIVELIB)

$(BYTELIB): $(CMO)
	$(CAMLC) -c -no-alias-deps $(LIBNAME).ml
	$(CAMLC) -a -o $(LIBNAME).cma $(LIBNAME).cmo $^

$(NATIVELIB): $(CMX)
	$(CAMLOPT) -c -no-alias-deps -o $(LIBNAME).cmx $(LIBNAME).ml
	$(CAMLOPT) -a -o $(LIBNAME).cmxa $(LIBNAME).cmx $^

.POSIX:

.PHONY: all clean doc

SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLC) $(CAMLFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(CAMLFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(CAMLFLAGS) -c $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) $(CAMLFLAGS) -c $*.mli
	$(CAMLC) $(CAMLFLAGS) -c $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) $(CAMLFLAGS) -c $*.mli
	$(CAMLOPT) $(CAMLFLAGS) -c $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) $(CAMLFLAGS) -c $*.mli
	$(CAMLC) $(CAMLFLAGS) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -c $*.mli
	$(CAMLOPT) -c $*.ml

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

clean:
	rm -f *.cm[iox] *~ .*~ *.o *.cma *.cmxa *.a
	rm -rf $(DOCDIR)

.depend: $(SOURCES2)
	$(CAMLDEP) *.mli *.ml > .depend

depend: $(SOURCES2) .depend
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

doc: $(BYTELIB)
	mkdir -p $(DOCDIR)
	$(CAMLDOC) -d $(DOCDIR) -I `$(OCAMLFIND) query llvm` -I . -html *.mli

install: $(BYTELIB) $(NATIVELIB) $(CMILIB)
	$(OCAMLFIND) install $(LIBNAME) META $^

uninstall:
	ocamlfind remove ollvm
