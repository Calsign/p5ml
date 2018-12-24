
module Bezier = struct
  type point = int * int
  type t = point * point * point * point

  let create x1 y1 x2 y2 x3 y3 x4 y4 = (x1, y1), (x2, y2), (x3, y3), (x4, y4)
  let of_points point1 point2 point3 point4 = point1, point2, point3, point4

  let anchor1 (point, _, _, _) = point
  let control1 (_, point, _, _) = point
  let control2 (_, _, point, _) = point
  let anchor2 (_, _, _, point) = point

  let interpolate ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) t =
    let t' = 1. -. t
    in let calc a b c d =
         (a *. t' +. 3. *. b *. t) *. (t' ** 2.)
         +. (3. *. c *. t' +. d *. t') *. (t ** 2.)
    in let wrap a b c d =
         calc (float_of_int a) (float_of_int b)
           (float_of_int c) (float_of_int d) |> int_of_float
    in wrap x1 x2 x3 x4, wrap y1 y2 y3 y4

  let tangent ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) t =
    let calc a b c d =
      3. *. (t ** 2.) *. (~-.a +. 3. *. b -. 3. *. c +. d)
      +. 6. *. t *. (a -. 3. *. b +. c) +. 3. *. (~-.a +. b)
    in let wrap a b c d =
         calc (float_of_int a) (float_of_int b)
           (float_of_int c) (float_of_int d)
    in wrap x1 x2 x3 x4, wrap y1 y2 y3 y4
end
