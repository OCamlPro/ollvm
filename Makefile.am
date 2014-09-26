if BUILD_LLVMGATEWAY

all:
	make -C src/ollvm
	make -C src/ollvm_llvmgateway

install:
	make -C src/ollvm install
	make -C src/ollvm_llvmgateway install

uninstall:
	make -C src/ollvm uninstall
	make -C src/ollvm_llvmgateway uninstall

doc:
	make -C src/ollvm doc
	make -C src/ollvm_llvmgateway doc

else

all:
	make -C src/ollvm

uninstall:
	make -C src/ollvm uninstall

doc:
	make -C src/ollvm doc

endif
