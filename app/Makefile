LAUNCHER=launcher.byte
NATIVE=launcher.native
OCAMLBUILD=ocamlbuild -use-ocamlfind -tag linkall

default: launcher

launcher:
	$(OCAMLBUILD) $(LAUNCHER)

native:
	$(OCAMLBUILD) $(NATIVE)

clean:
	ocamlbuild -clean
	rm -rf sketch
