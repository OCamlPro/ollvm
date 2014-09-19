if BUILD_LLVMGATEWAY
  LIB=ollvm
else
  LIB=ollvm ollvm_llvmgateway
endif

DOCDIR=doc

all: $(LIB)

$(LIB):
	$(OCPBUILD) init && $(OCPBUILD) $(LIB)

install: $(LIB)
	$(OCPBUILD) install $(LIB)

uninstall:
	ocamlfind remove $(LIB)

doc: $(LIB)
	mkdir -p doc ; ocamldoc -d $(DOCDIR) -html -I _obuild/ollvm src/*.mli

clean:
	$(OCPBUILD) clean
	rm -rf $(DOCDIR)
