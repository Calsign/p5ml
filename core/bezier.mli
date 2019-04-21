
open Math

module Bezier : sig
  type vector = Vector.t
  type t = vector * vector * vector * vector

  val create : vector -> vector -> vector -> vector -> t

  val anchor1 : t -> vector
  val control1 : t -> vector
  val control2 : t -> vector
  val anchor2 : t -> vector

  val interpolate : t -> float -> vector
  val tangent : t -> float -> vector
end
