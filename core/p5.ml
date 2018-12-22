
module Core = struct
  include Config
  include Color
  include Key
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
