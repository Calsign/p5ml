
## p5ml

An implementation of [Processing](https://processing.org/) in
[OCaml](https://ocaml.org/), driven by the functional paradigm. The goal of
this project is to reimagine Processing in a purely functional context to
bridge the gap between the learning potential of Processing and the functional
nature of OCaml.

Read the auto-generated docs: [API](https://calsign.github.io/p5ml/)

To build the p5ml library, run `make` in the `core` directory. This takes care
of linking the p5ml library to ocamlfind. You may need to install some
libraries with opam (ounit, graphics, lablgtk, cairo2, cairo2-gtk).

To build a sketch, first run `make` in the `app` directory. Then pass the
sketch to `app/launcher.byte` (e.g. `app/launcher.byte examples/demo.ml`) in
order to run the sketch. There are examples in the `examples` directory.

> Tip: pass the `-d` flag to `app/launcher.byte` (dynamic mode) to make the
> sketch reload automatically when the file is modified.
