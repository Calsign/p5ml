
(** [Bezier] contains the [Bezier.t] type, which stores information about
    a cubic Bezier curve, and functions for creating and manipulating
    Bezier curves. *)

open Math

type vector = Vector.t

(** [(a1, c1, c2, a2)] is the Bezier curve with anchor points [a1] and [a2]
    and control points [c1] and [c2]. *)
type t = vector * vector * vector * vector

(** {2 Bezier Creation} *)

(** [create a1 c1 c2 a2] is [(a1, c1, c2, a2)]. *)
val create : vector -> vector -> vector -> vector -> t

(** {2 Bezier Helper Functions} *)

(** [anchor1 bez] is the first anchor point of [bez]. *)
val anchor1 : t -> vector

(** [control1 bez] is the first control point of [bez]. *)
val control1 : t -> vector

(** [control2 bez] is the second control point of [bez]. *)
val control2 : t -> vector

(** [anchor2 bez] is the second anchor point of [bez]. *)
val anchor2 : t -> vector

(** {2 Bezier Manipulation} *)

(** [interpolate bez time] is the point at [time] along [bez], where [time]
    is in [[0, 1]]. *)
val interpolate : t -> float -> vector

(** [tangent bez time] is the vector tanget to [bez] at [time], where [time]
    is in [[0, 1]]. *)
val tangent : t -> float -> vector
