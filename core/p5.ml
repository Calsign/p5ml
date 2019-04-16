
module Core = struct
  include Math
  include Config
  include Color
  include Key
  include Bezier
  include Runner

  module type Sketch = Sketch.Sketch
end

module Maker (R : Renderer.Renderer) = struct
  include Core

  open R

  module Base = Sketch.Base (R)
  module Canvas = Canvas.Canvas (R)

  include Canvas
end

module Graph = Maker (Graph.Graph)
module Gtkc = Maker (Gtk_cairo.Gtk_cairo)
