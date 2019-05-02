
module Math = struct
  let pi = 2. *. Pervasives.asin 1.
  let half_pi = pi /. 2.
  let two_pi = pi *. 2.

  let e = Pervasives.exp 1.

  let abs = Pervasives.abs
  let absf = Pervasives.abs_float
  let ceil v = Pervasives.ceil v |> int_of_float
  let ceilf = Pervasives.ceil
  let floor v = Pervasives.floor v |> int_of_float
  let floorf = Pervasives.floor
  let constrain (v : int) lower upper =
    Pervasives.max v lower |> Pervasives.min upper
  let constrainf (v : float) lower upper =
    Pervasives.max v lower |> Pervasives.min upper
  let distf x1 y1 x2 y2 = sqrt ((x2 -. x1) ** 2. +. (y2 -. y1) ** 2.)
  let dist x1 y1 x2 y2 = distf (float_of_int x1) (float_of_int y1)
      (float_of_int x2) (float_of_int y2)
  let mag x y = dist x y 0 0
  let magf x y = distf x y 0. 0.
  let lerpf lower upper amt = (upper -. lower) *. amt
  let lerp lower upper amt =
    lerpf (float_of_int lower) (float_of_int upper) amt |> int_of_float
  let log base v = (log v) /. (log base)
  let mapf v from_lower from_upper to_lower to_upper =
    lerpf to_lower to_upper (v /. (from_lower -. from_upper))
  let map v from_lower from_upper to_lower to_upper =
    mapf (float_of_int v) (float_of_int from_lower) (float_of_int from_upper)
      (float_of_int to_lower) (float_of_int to_upper) |> int_of_float
  let max (a : int) (b : int) = Pervasives.max a b
  let maxf (a : float) (b : float) = Pervasives.max a b
  let min (a : int) (b : int) = Pervasives.min a b
  let minf (a : float) (b : float) = Pervasives.min a b
  let normf v lower upper = mapf v lower upper 0. 1.
  let norm v lower upper = normf (float_of_int v)
      (float_of_int lower) (float_of_int upper)
  let round v = Pervasives.floor (v +. (if v > 0. then 0.5 else -0.5))
                |> int_of_float
  let sqrt = Pervasives.sqrt

  let acos = Pervasives.acos
  let asin = Pervasives.asin
  let atan = Pervasives.atan
  let atan2 = Pervasives.atan2
  let cos = Pervasives.cos
  let sin = Pervasives.sin
  let tan = Pervasives.tan
  let degrees r = r *. 180. /. pi
  let radians d = d *. pi /. 180.

  let angle_avg a b = atan2 (sin a +. sin b) (cos a +. cos b)
  let angle_sum a b = mod_float (a +. b) (pi *. 2.)
  let angle_diff a b = atan2 (sin (a -. b)) (cos (a -. b))

  let () = Random.self_init ()

  let random bound = Random.int bound
end

let (~.) = float_of_int

module Vector = struct
  type t = float * float

  let create x y = (x, y)
  let of_tuple (x, y) = create x y
  let of_angle theta = create (Math.cos theta) (Math.sin theta)

  let (~<) (x, y) = x
  let (~>) (x, y) = y

  let mag_sq (x, y) = x ** 2. +. y ** 2.
  let mag vec = mag_sq vec |> Math.sqrt
  let add (x1, y1) (x2, y2) = create (x1 +. x2) (y1 +. y2)
  let sub (x1, y1) (x2, y2) = create (x1 -. x2) (y1 -. y2)
  let mult (x, y) scalar = create (x *. scalar) (y *. scalar)
  let div vec scalar = mult vec (1. /. scalar)
  let dist (x1, y1) (x2, y2) = Math.distf x1 y1 x2 y2
  let dot (x1, y1) (x2, y2) = x1 *. x2 +. y1 *. y2
  let norm vec = div vec (mag vec)
  let with_mag vec scalar = mult (norm vec) scalar
  let limit vec lim =
    let curr_mag = mag vec
    in if curr_mag > lim then with_mag vec lim else vec
  let heading (x, y) = Math.atan2 y x
  let rotate (x, y) theta =
    create (x *. (Math.cos theta) -. y *. (Math.sin theta))
      (x *. (Math.sin theta) +. y *. (Math.cos theta))
  let lerp (x1, y1) (x2, y2) amt =
    create (Math.lerpf x1 x2 amt) (Math.lerpf y1 y2 amt)
  let angle_between vec1 vec2 =
    Math.acos ((dot vec1 vec2) /. ((mag vec1) *. (mag vec2)))
  let project vec onto = mult onto ((dot vec onto) /. (mag_sq onto))

  let to_string (x, y) = Printf.sprintf "(%f,%f)" x y

  let (++) = add
  let (--) = sub
  let ( *** ) = mult
  let (//) = div
  let ( **. ) = dot
  let (~||) = mag
end
