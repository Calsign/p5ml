
(** [Paint] contains the [paint] type and utilities for manipulating [paint]s.
    This module should not be used directly by the sketch; there are functions
    in [Canvas] that handle setting the stroke, fill, etc. *)

open Color

(** The type of paint, used by painters to draw to the canvas. *)
type paint

val fill : color -> paint -> paint

val stroke : color -> paint -> paint

val no_fill : paint -> paint

val no_stroke : paint -> paint

val stroke_weight : float -> paint -> paint

val extr_fill : paint -> color option

val extr_stroke : paint -> color option

val extr_stroke_weight : paint -> float

(** [create] is the default paint. *)
val create : paint
