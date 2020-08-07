(** [P5] is the main entrypoint to p5ml. *)

(** [Core] is a wrapper module around the core p5ml modules, except for
   the renderers. The modules below support different renderers. *)
module Core = struct
  include Misc
  include Math
  include Config
  include Color
  include Key
  include Shape
  include Runner

  module Dict = Map.Make (String)

  module type Sketch = Sketch.Sketch
end

(** [Maker R] is a p5ml core module instantiated with the renderer
   [R]. Most likely you want to use [Gtkc] below, which is
   instantiated with the GTK+/cairo2 renderer. *)
module Maker (R : Renderer.Renderer) = struct
  include Core

  (** [Base] contains default implementations of most of the [Sketch]
     functions; use it by including it at the top of your sketch
     module. *)
  module Base = Sketch.Base (R)
end

(** [Graph] is the p5ml core module instantiated with the GTK+/cairo2
   renderer. *)
module Graph = Maker (Graph.Graph)

(** [Gtkc] is the p5ml core module instantiated with the Graphics
   renderer. *)
module Gtkc = Maker (Gtk_cairo.Gtk_cairo)
