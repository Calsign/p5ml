MODULES=color paint
RAW_MLS=key renderer canvas graph gtk_cairo config sketch runner p5
COMPILES=$(MODULES) $(RAW_MLS)
OBJECTS=$(COMPILES:=.cmo)
MLS=$(MODULES:=.ml) $(RAW_MLS:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
LIB_BYTE=p5.cma
LIB_OPT=p5.cmxa
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,graphics

default: lib install

build:
	$(OCAMLBUILD) $(OBJECTS)

lib: lib-byte lib-opt

lib-byte:
	$(OCAMLBUILD) $(LIB_BYTE)

lib-opt:
	$(OCAMLBUILD) $(LIB_OPT)

install:
	ocamlfind remove p5ml
	ocamlfind install p5ml META _build/p5.cma _build/p5.cmxa _build/*.cmi _build/*.cmx _build/*.a

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST)
	./$(TEST)

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private

uninstall:
	ocamlfind remove p5ml