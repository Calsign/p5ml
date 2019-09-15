
open P5.Gtkc

module Hsv : Sketch = struct
  include Base

  type state = unit

  let display = `Size (400, 400)

  let setup _ = ()

  let draw conf _ =
    let h = conf.frame_count mod 360 in
    let s = ~.(conf.mouse_x) /. ~.(conf.width) *. 100. |> Math.round in
    let v = ~.(conf.mouse_y) /. ~.(conf.height) *. 100. |> Math.round in
    background (hsv h s v)
end

let () = run_sketch (module Hsv)
