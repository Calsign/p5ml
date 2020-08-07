
(** [Math] contains standard mathematical functions, trignometric functions,
    operations on angles, vectors, and operations on vectors. *)

(** [~. i] is [float_of_int i]. *)
val (~.) : int -> float

(** [Math] contains many common mathematical functions. *)
module Math : sig
  (** [abs i] is the absolute value of [i]. *)
  val abs : int -> int

  (** [absf f] is the absolute value of [f]. *)
  val absf : float -> float

  (** [ceil f] is the integer closest to [f] in the direction of positive
      infinity. *)
  val ceil : float -> int

  (** [floor f] is the integer closest to [f] in the direction of negative
      infinity. *)
  val floor : float -> int

  (** [constrain i min max] is [min] if [i <= min], [max] if [i >= max],
      or [i] otherwise. The behavior is undefined if [min > max]. *)
  val constrain : int -> int -> int -> int

  (** [constrainf f min max] is [min] if [f <= min], [max] if [f >= max],
      or [f] otherwise. The behavior is undefined if [min > max]. *)
  val constrainf : float -> float -> float -> float

  (** [dist x1 y1 x2 y2] is the distance between the points [(x1, y1)] and
      [(x2, y2)]. *)
  val dist : int -> int -> int -> int -> float

  (** [distf x1 y1 x2 y2] is the distance between the points [(x1, y1)] and
      [(x2, y2)]. *)
  val distf : float -> float -> float -> float -> float

  (** [mag x y] is [dist x y 0 0]. *)
  val mag : int -> int -> float

  (** [magf x y] is [distf x y 0 0]. *)
  val magf : float -> float -> float

  (** [lerp a b t] is the linear interpolation from [a] to [b] by [t], where
      [t] is in [[0, 1]]. *)
  val lerp : int -> int -> float -> int

  (** [lerpf a b t] is the linear interpolation from [a] to [b] by [t], where
      [t] is in [[0, 1]]. *)
  val lerpf : float -> float -> float -> float

  (** [log f b] is the base-[b] logarithm of [f]. *)
  val log : float -> float -> float

  (** [map i low1 high1 low2 high2] is [i] mapped from the range [[low1, high1]]
      to the range [[low2, high2]]. *)
  val map : int -> int -> int -> int -> int -> int

  (** [mapf f low1 high1 low2 high2] is [f] mapped from the range
      [[low1, high1]] to the range [[low2, high2]]. *)
  val mapf : float -> float -> float -> float -> float -> float

  (** [max a b] is the maximum of [a] and [b]. *)
  val max : int -> int -> int

  (** [maxf a b] is the maximum of [a] and [b]. *)
  val maxf : float -> float -> float

  (** [min a b] is the minimum of [a] and [b]. *)
  val min : int -> int -> int

  (** [minf a b] is the minimum of [a] and [b]. *)
  val minf : float -> float -> float

  (** [norm i low high] is [map i low high 0 1]. *)
  val norm : int -> int -> int -> float

  (** [normf f low high] is [mapf f low high 0. 1.]. *)
  val normf : float -> float -> float -> float

  (** [round f] is the integer closest to [f]; [0.5] rounds up. *)
  val round : float -> int

  (** [sqrt f] is the square root of [f]. *)
  val sqrt : float -> float

  (** {2 Trigonometry} *)

  (** All functions in this section expect angles in radians. *)

  (** [acos f] is the inverse cosine of [f]. [f] is in [[-1, 1]]
      and [acos f] is in [[0, pi]]. *)
  val acos : float -> float

  (** [asin f] is the inverse sine of [f]. [f] is in [[-1, 1]] and
      [asin f] is in [[-pi/2, pi/2]]. *)
  val asin : float -> float

  (** [atan f] is the inverse tangent of [f]. [f] is in
      [[-infinity, infinity]] and [atan f] is in [[-pi/2, pi/2]]. *)
  val atan : float -> float

  (** [atan2 y x] is the angle from the origin to [(y, x)]. This function
      uses the signs of [x] and [y] to determine the correct quadrant. *)
  val atan2 : float -> float -> float

  (** [cos theta] is the cosine of [theta]. *)
  val cos : float -> float

  (** [sin theta] is the sine of [theta]. *)
  val sin : float -> float

  (** [tan theta] is the tangent of [theta]. *)
  val tan : float -> float

  (** {2 Angle Conversion} *)

  (** [degrees theta] is [theta], which is in radians, converted to degrees. *)
  val degrees : float -> float

  (** [radians d] is [d], which is in degrees, converted to radians. *)
  val radians : float -> float

  (** {2 Angle Manipulation} *)

  (** All functions in this section expect angles in radians. *)

  (** [angle_avg theta1 theta2] is the average angle between [theta1] and
      [theta2]. *)
  val angle_avg : float -> float -> float

  (** [angle_sum theta1 theta2] is the sum of the angles [theta1] and
      [theta2]. *)
  val angle_sum : float -> float -> float

  (** [angle_diff theta1 theta2] is the difference between the angles [theta1]
      and [theta2]. *)
  val angle_diff : float -> float -> float

  (** {2 Mathematical Constants} *)

  (** [pi] is the constant pi. [pi = 3.1415926535...] *)
  val pi : float

  (** [half_pi] is [pi /. 2.]. [half_pi = 1.5707963267...] *)
  val half_pi : float

  (** [two_pi] is [pi *. 2.]. [two_pi = 6.2831853071...] *)
  val two_pi : float

  (** [e] is the constant e. [e = 2.7182818284...] *)
  val e : float

  (** {2 Random} *)

  (** [random_int bound] is a random integer between [0] (inclusive) and
      [bound] (exclusive). *)
  val random_int : ?lower_bound : int -> int -> int

  (** [random_float bound] is a random float between [0.] (inclusive) and
      [bound] (exclusive). *)
  val random_float : ?lower_bound : float -> float -> float

  (** [random_bool ()] is either [true] or [false] with an equal probability
      of either. *)
  val random_bool : unit -> bool
end

(** [Vector] contains the [Vector.t] type and functions for
    creating and manipulating vectors. *)
module Vector : sig
  type t = float * float

  (** [to_string vec] is the string representation of [vec]. *)
  val to_string : t -> string

  (** [~< (x, y)] is [x]. *)
  val (~<) : t -> float

  (** [~> (x, y)] is [y]. *)
  val (~>) : t -> float

  (** {2 Creation} *)

  (** [create x y] is [(x, y)]. *)
  val create : float -> float -> t

  (** [of_tuple (x, y)] is [(x, y)]. *)
  val of_tuple : (float * float) -> t

  (** [of_angle theta] is the unit vector [(cos theta, sin theta)]. *)
  val of_angle : float -> t

  (** {2 Manipulation} *)

  (** [mag vec] is the magnitude of [vec], i.e. [sqrt (x**2. +. y**2.)] *)
  val mag : t -> float

  (** [mag_sq vec] is the squared magnitude of [vec], i.e. [x**2. +. y**2.].
      This computation is faster than [mag]. *)
  val mag_sq : t -> float

  (** [add v1 v2] is the sum of [v1] and [v2]. *)
  val add : t -> t -> t

  (** [sub v1 v2] is the difference between [v1] and [v2], which is
      equivalent to [add v1 (mult v2 (-1.))]. *)
  val sub : t -> t -> t

  (** [mult vec f] is the scalar multiplication of [vec] by [f]. *)
  val mult : t -> float -> t

  (** [div vec f] is [mult vec (1. /. f)]. *)
  val div : t -> float -> t

  (** [dist v1 v2] is the distance between [v1] and [v2], which is equivalent
      to [mag (sub v1 v2)]. *)
  val dist : t -> t -> float

  (** [dot v1 v2] is the dot product of [v1] and [v2], i.e.
      [(x1 *. x2) +. (y1 *. y2)], where [v1 = (x1, y1)] and [v2 = (x2, y2)]. *)
  val dot : t -> t -> float

  (** [norm vec] is [vec] with a magnitude of [1.], which is equivalent to
      [div vec (mag vec)]. *)
  val norm : t -> t

  (** [with_mag vec f] is [vec] with a magnitude of [f], which is equivalent
      to [mult (norm vec) f]. *)
  val with_mag : t -> float -> t

  (** [limit vec f] is [vec] with a maximum magnitude of [f], which is
      equivalent to [with_mag vec (min (mag vec) f)]. *)
  val limit : t -> float -> t

  (** [heading vec] is the heading of [vec], i.e. [atan2 y x] where
      [vec = (x, y)]. *)
  val heading : t -> float

  (** [rotate vec theta] is [vec] rotated by angle [theta]. *)
  val rotate : t -> float -> t

  (** [lerp v1 v2 t] is the linear interpolation from [v1] to [v2] by [t],
      where [t] is in [[0, 1]]. *)
  val lerp : t -> t -> float -> t

  (** [angle_between v1 v2] is the smallest angle between [v1] and [v2]. *)
  val angle_between : t -> t -> float

  (** [project v1 v2] is the projection of [v1] on to [v2]. *)
  val project : t -> t -> t

  (** {2 Operator Aliases} *)

  (** [v1 ++ v2] is [add v1 v2]. *)
  val (++) : t -> t -> t

  (** [v1 -- v2] is [sub v1 v2]. *)
  val (--) : t -> t -> t

  (** [vec *** f] is [mult vec f]. *)
  val ( *** ) : t -> float -> t

  (** [vec // f] is [div vec f].  *)
  val (//) : t -> float -> t

  (** [v1 **. v2] is [dot v1 v2]. *)
  val ( **. ) : t -> t -> float

  (** [~|| vec] is [mag vec]. *)
  val (~||) : t -> float
end

type vector = Vector.t
