
(** [Paint] contains the [paint] type and utilities for manipulating [paint]s.
    This module should not be used directly by the sketch; there are functions
    in [Shape] that handle setting the stroke, fill, etc. *)

open Color

(** The type of paint, used by shapes to draw to the canvas. *)
type paint = private {
  fill : color option;
  stroke : color option;
  stroke_weight : float;
  stroke_cap : [`Round | `Square | `Project];
  stroke_join : [`Miter | `Bevel | `Round];
}

(** [create] is the default paint. *)
val create : paint

type paint_update =
  | Fill of color option
  | Stroke of color option
  | Stroke_weight of float
  | Stroke_cap of [`Round | `Square | `Project]
  | Stroke_join of [`Miter | `Bevel | `Round]

val apply_paint_update : paint_update -> paint -> paint
