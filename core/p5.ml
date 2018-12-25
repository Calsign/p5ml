
module Core = struct
  include Math
  include Config
  include Color
  include Key
  include Bezier
  include Shape
  include Runner

  module type Sketch = Sketch.Sketch
end

module Graph = struct
  include Core

  open Graph

  module Base = Sketch.Base (Graph)
  module Canvas = Canvas.Canvas (Graph)

  include Canvas
end

module Gtkc = struct
  include Core

  open Gtk_cairo

  module Base = Sketch.Base (Gtk_cairo)
  module Canvas = Canvas.Canvas (Gtk_cairo)

  include Canvas
end
