LAUNCHER=launcher.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: launcher

launcher:
	$(OCAMLBUILD) $(LAUNCHER)

clean:
	ocamlbuild -clean
	rm -rf sketch
