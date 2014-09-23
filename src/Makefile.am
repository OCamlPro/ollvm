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

LIBNAME=ollvm

SOURCES = ast.ml parser.mly lexer.mll printer.ml ez.ml
CMILIB = $(LIBNAME).cmi ast.cmi ez.cmi printer.cmi lexer.cmi
CAMLFLAGS =-for-pack Ollvm

if BUILD_LLVMGATEWAY
  SOURCES += llvmgateway.ml
  CMILIB += llvmgateway.cmi
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
	$(CAMLC) -pack -o $(LIBNAME).cmo $^
	$(CAMLC) -a -o $(LIBNAME).cma $(LIBNAME).cmo $^

$(NATIVELIB): $(CMX)
	$(CAMLOPT) -pack -o $(LIBNAME).cmx $^
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
	$(CAMLC) $(CAMLFLAGS) -c $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) $(CAMLFLAGS) -c $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) $(CAMLFLAGS) -c $*.mli
	$(CAMLC) $(CAMLFLAGS) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -c $*.mli
	$(CAMLOPT) -c $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) $(CAMLFLAGS) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

clean:
	rm -f *.cm[iox] *~ .*~ *.o *.cma *.cmxa *.a
	rm -rf $(DOCDIR)

.depend: $(SOURCES2)
	$(CAMLDEP) *.mli *.ml > .depend

depend: $(SOURCES2)
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

doc: $(BYTELIB) ez.mli
	mkdir -p $(DOCDIR) ; $(OCAMLDOC) -d $(DOCDIR) -I . -html ez.mli

install: $(BYTELIB) $(NATIVELIB) $(CMILIB)
	$(OCAMLFIND) install $(LIBNAME) META $^

uninstall:
	ocamlfind remove ollvm
