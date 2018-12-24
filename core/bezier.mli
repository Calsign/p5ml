
module Bezier : sig
  type point = int * int
  type t = point * point * point * point

  val create : int -> int -> int -> int -> int -> int -> int -> int -> t
  val of_points : point -> point -> point -> point -> t

  val anchor1 : t -> point
  val control1 : t -> point
  val control2 : t -> point
  val anchor2 : t -> point

  val interpolate : t -> float -> point
  val tangent : t -> float -> float * float
end
