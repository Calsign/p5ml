
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

module Maker (R : Renderer.Renderer) = struct
  include Core
  module Base = Sketch.Base (R)
end

module Graph = Maker (Graph.Graph)
module Gtkc = Maker (Gtk_cairo.Gtk_cairo)
