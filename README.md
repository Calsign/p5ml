
## p5ml

An implementation of [Processing](https://processing.org/) in
[OCaml](https://ocaml.org/), driven by the functional paradigm. The goal of
this project is to reimagine Processing in a purely functional context to
bridge the gap between the learning potential of Processing and the functional
nature of OCaml.

To build the p5ml library, run `make` in the `core` directory. This takes care
of linking the p5ml library to ocamlfind. You may need to install some
libraries with opam (OUnit, Graphics).

To build a sketch, first run `make` in the `app` directory. Then pass the
sketch to `app/launcher.byte` (e.g. `app/launcher.byte examples/demo.ml`) in
order to run the sketch. There are examples in the `examples` directory.
