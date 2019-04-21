
module Math : sig
  val abs : int -> int
  val absf : float -> float
  val ceil : float -> int
  val floor : float -> int
  val constrain : int -> int -> int -> int
  val constrainf : float -> float -> float -> float
  val dist : int -> int -> int -> int -> float
  val distf : float -> float -> float -> float -> float
  val mag : int -> int -> float
  val magf : float -> float -> float
  val lerp : int -> int -> float -> int
  val lerpf : float -> float -> float -> float
  val log : float -> float -> float
  val map : int -> int -> int -> int -> int -> int
  val mapf : float -> float -> float -> float -> float -> float
  val max : int -> int -> int
  val maxf : float -> float -> float
  val min : int -> int -> int
  val minf : float -> float -> float
  val norm : int -> int -> int -> float
  val normf : float -> float -> float -> float
  val round : float -> int
  val sqrt : float -> float

  val acos : float -> float
  val asin : float -> float
  val atan : float -> float
  val atan2 : float -> float -> float
  val cos : float -> float
  val sin : float -> float
  val tan : float -> float
  val degrees : float -> float
  val radians : float -> float

  val pi : float
  val half_pi : float
  val two_pi : float

  val e : float
end

val (~.) : int -> float

module Vector : sig
  type t = float * float

  val create : float -> float -> t
  val of_tuple : (float * float) -> t
  val of_angle : float -> t

  val (~<) : t -> float
  val (~>) : t -> float

  val mag : t -> float
  val mag_sq : t -> float
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> float -> t
  val div : t -> float -> t
  val dist : t -> t -> float
  val dot : t -> t -> float
  val norm : t -> t
  val limit : t -> float -> t
  val with_mag : t -> float -> t
  val heading : t -> float
  val rotate : t -> float -> t
  val lerp : t -> t -> float -> t
  val angle_between : t -> t -> float
  val project : t -> t -> t

  val (++) : t -> t -> t
  val (--) : t -> t -> t
  val ( ** ) : t -> float -> t
  val (//) : t -> float -> t
  val ( **. ) : t -> t -> float
  val (~||) : t -> float
end
