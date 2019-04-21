
open Math

module Bezier = struct
  type vector = Vector.t
  type t = vector * vector * vector * vector

  let create vec1 vec2 vec3 vec4 = vec1, vec2, vec3, vec4

  let anchor1 (vec1, _, _, _) = vec1
  let control1 (_, vec2, _, _) = vec2
  let control2 (_, _, vec3, _) = vec3
  let anchor2 (_, _, _, vec4) = vec4

  let interpolate ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) t =
    let t' = 1. -. t
    in let calc a b c d =
         (a *. t' +. 3. *. b *. t) *. (t' ** 2.)
         +. (3. *. c *. t' +. d *. t) *. (t ** 2.)
    in calc x1 x2 x3 x4, calc y1 y2 y3 y4

  let tangent ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) t =
    let calc a b c d =
      3. *. (t ** 2.) *. (~-.a +. 3. *. b -. 3. *. c +. d)
      +. 6. *. t *. (a -. 2. *. b +. c) +. 3. *. (~-.a +. b)
    in calc x1 x2 x3 x4, calc y1 y2 y3 y4
end
