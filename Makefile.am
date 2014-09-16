LIB=ollvm ollvm_llvmgateway

all:
	$(OCPBUILD) init && $(OCPBUILD) $(LIB)

install:
	$(OCPBUILD) install $(LIB)

uninstall:
	ocamlfind remove $(LIB)

clean:
	$(OCPBUILD) clean
