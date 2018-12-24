
(** [Paint] contains the [paint] type and utilities for manipulating [paint]s.
    This module should not be used directly by the sketch; there are functions
    in [Canvas] that handle setting the stroke, fill, etc. *)

open Color

(** The type of paint, used by painters to draw to the canvas. *)
type paint = private {
  fill : color option;
  stroke : color option;
  stroke_weight : float;
  stroke_cap : [`Round | `Square | `Project];
  stroke_join : [`Miter | `Bevel | `Round];
}

val fill : color -> paint -> paint

val stroke : color -> paint -> paint

val no_fill : paint -> paint

val no_stroke : paint -> paint

val stroke_weight : float -> paint -> paint

val stroke_cap : [`Round | `Square | `Project] -> paint -> paint

val stroke_join : [`Miter | `Bevel | `Round] -> paint -> paint

(** [create] is the default paint. *)
val create : paint
