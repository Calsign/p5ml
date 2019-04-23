LAUNCHER=launcher.byte
NATIVE=launcher.native
OCAMLBUILD=ocamlbuild -use-ocamlfind -tag linkall stubs.o -lflags -custom,stubs.o

default: launcher

launcher:
	$(OCAMLBUILD) $(LAUNCHER)

native:
	$(OCAMLBUILD) $(NATIVE)

clean:
	ocamlbuild -clean
	rm -rf sketch

fresh: clean default
