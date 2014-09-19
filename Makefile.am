if BUILD_LLVMGATEWAY
  LIB=ollvm
else
  LIB=ollvm ollvm_llvmgateway
endif

all: $(LIB)

$(LIB):
	$(OCPBUILD) init && $(OCPBUILD) $(LIB)

install:
	$(OCPBUILD) install $(LIB)

uninstall:
	ocamlfind remove $(LIB)

doc-html: $(LIB)
	mkdir -p doc ; ocamldoc -d doc -html -I _obuild/ollvm src/*.mli

clean:
	$(OCPBUILD) clean
