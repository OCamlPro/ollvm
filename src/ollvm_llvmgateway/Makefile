LIBNAME=ollvm_llvmgateway

SOURCES = $(LIBNAME).ml
CMI = $(LIBNAME).cmi
CAMLFLAGS = -I ../ollvm -package llvm

include ../Makefile.common
include .depend

$(BYTE): $(CMO)
	$(CAMLC) $(CAMLFLAGS) -a -o $(LIBNAME).cma $^

$(NATIVE): $(CMX)
	$(CAMLOPT) $(CAMLFLAGS) -a -o $(LIBNAME).cmxa $^
