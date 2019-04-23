
open Color
open Paint
open Math
open Bezier

type vector = Vector.t

type vertex = private
  | MoveTo of vector
  | LineTo of vector
  | BezierTo of vector * vector * vector
  | Arc of vector * vector * float * float * float
  | ClosePath

type t = private
  | Shape of vertex list
  | Group of t list
  | Paint of t * paint_update
  | Name of t * string
  | Background of color
  | Empty

val poly : ?close : [`Close | `Open] -> vector list -> t
val point : vector -> t
val line : vector -> vector -> t
val rect : vector -> ?align : [`Corner | `Center] -> vector -> t
val quad : vector -> vector -> vector -> vector -> t
val triangle : vector -> vector -> vector -> t
val ellipse : vector -> ?align : [`Corner | `Center] -> vector -> t
val circle : vector -> ?align : [`Corner | `Center] -> float -> t
val arc : vector -> ?align : [`Corner | `Center] -> vector ->
  ?stroke_mode : [`Closed | `Open] ->
  ?fill_mode : [`Pie | `Chord] -> ?phi : float -> float -> float -> t
val bezier : Bezier.t -> t

val group : t list -> t
val shape : t -> vector -> t
val background : color -> t
val empty : t

val fill : color -> t -> t
val no_fill : t -> t
val stroke : color -> t -> t
val no_stroke : t -> t
val stroke_weight : float -> t -> t
val stroke_cap : [`Round | `Square | `Project] -> t -> t
val stroke_join : [`Miter | `Bevel | `Round] -> t -> t

val name : string -> t -> t

val find_named : t -> string -> t option

val translate : vector -> t -> t
val scale : vector -> t -> t
val rotate : float -> t -> t
