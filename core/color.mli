
(** [Color] contains the [color] type and utilities for manipulating
    [color]s. *)

(** The type of a color. Each of [red], [green], [blue], and [alpha]
    are in the range [0..255]. *)
type color = {
  red : int;
  green : int;
  blue : int;
  alpha : int;
}

(** [rgb r g b] is the opaque color with red, green and blue values of [r], [g],
    and [b], respectively. *)
val rgb : int -> int -> int -> color

(** [rgba r g b a] is [rgb r g b], but with an alpha value of [a]. *)
val rgba : int -> int -> int -> int -> color

(** [gray val] is the opaque color represented by the grayscale value [val],
    which is equivalent to each of red, green, and blue values of [val]. *)
val gray : int -> color

(** [gray val a] is [gray val], but with an alpha value of [a]. *)
val graya : int -> int -> color

(** [hsv h s v] is the opaque color represented by the hue, saturation, and
    value values of [h], [s], and [v], respectively. [h] is in the range
    [0..360], and both [s] and [v] are in the range [0..100]. *)
val hsv : int -> int -> int -> color

(** [hsva h s v a] is [hsv h s v], but with an alpha value of [a]. [a] is in the
    range [0..255]. *)
val hsva : int -> int -> int -> int -> color

(** [hex hex_str] is the color represented by [hex_str] in hexadecimal notation.
    [hex_str] may or may not begin with a pound ([#]), and may specify alpha
    ([#AARRGGBB]) or not ([#RRGGBB]). *)
val hex : string -> color
