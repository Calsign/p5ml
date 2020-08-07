
(** [Shape] contains the [Shape.t] type and utilities for creating and
    manipulating shapes. A [Shape.t] is a vector representation of a
    drawing command and serves as the basis for all drawing operations
    in p5ml. *)

open Color
open Paint
open Math
open Bezier

(** {2 Shapes} *)

(** [tag] is an extensible variant for attaching arbitrary metadata to
    shapes. *)
type tag = ..

type tag += TName of string

(** The type of a vertex, a single piece of vector information that
    forms a shape. *)
type vertex = private
  | MoveTo of vector
  (** [MoveTo vec] picks up the pen and places it down at [vec]. *)

  | LineTo of vector
  (** [LineTo vec] draws from the pen location to [vec]. *)

  | BezierTo of vector * vector * vector
  (** [BezierTo v2 v3 v4] draws from the pen location, using the pen location
      as the beginning anchor point, [v2] and [v3] as the anchor points, and
      [v4] as the ending anchor point. *)

  | Arc of vector * vector * float * float * float
  (** [Arc center size phi theta1 theta2] draws an arc centered at [center]
      and inscribed in [rect point ~align:`Center size]. [phi] is the offset
      angle of the rectangle, and [theta1] and [theta2] are the start and end
      angles of the arc, respectively. The pen is not moved to the start; it
      continues from where it left off and connects to where the arc begins. *)

  | ClosePath
  (** [ClosePath] closes the path by drawing a line from the current pen
      position to the last pen down position (created by [MoveTo]). *)

(** The type of a shape, a component that can be displayed.
    The [Shape] constructor contains vertices. *)
type t = private
  | Shape of vertex list
  (** [Shape vertices] is a complex shape defined by [vertices]. Well-formed
      shapes must begin with [MoveTo] and cannot have a [ClosePath] anywhere
      except (optionally) the end. *)

  | Group of t list
  (** [Group shapes] is the compound shape that draws each shape in [shapes]
      in order, i.e. with the last shape on top. *)

  | Paint of t * paint_update
  (** [Paint shape update] is the shape that draws [shape] with the paint
      modification specified by [update]. Paint updates are specified one
      attribute at a time (e.g. just the stroke) so that all other attributes
      are inherited from the parent shape. *)

  | Tag of t * tag
  (** [Tag shape tag] is [shape] tagged with [tag]. This shape is functionally
      equivalent to [shape], but the tag is used for adding arbitrary metadata
      to the shape, such as the built-in name tag. *)

  | Background of color
  (** [Background color] is the shape that fills the background with [color],
      erasing what was already present. This separate constructor is necessary
      because a [rect] would be affected by transformations. *)

  | Empty
  (** [Empty] is the shape that draws nothing. *)

(** {2 Constructing Basic Shapes} *)

(** [poly vertices] is the shape representing the polygon with [vertices]. *)
val poly : ?close : [`Close | `Open] -> vector list -> t

(** [point vertex] is the shape representing the point at position [vertex]. *)
val point : vector -> t

(** [line v1 v2] is the shape representing the line between [v1] and [v2]. *)
val line : vector -> vector -> t

(** [rect point size] is the shape representing the rectangle with corner at
    [point] and dimensions [size]. *)
val rect : vector -> ?align : [`Corner | `Center] -> vector -> t

(** [quad v1 v2 v3 v3] is [polygon [v1; v2; v3; v4]]. *)
val quad : vector -> vector -> vector -> vector -> t

(** [triangle v1 v2 v3] is [polygon [v1; v2; v3]]. *)
val triangle : vector -> vector -> vector -> t

(** [ellipse point size] is the shape representing the ellipse centered at
    [point] and inscribed in [rect point ~align:`Center size]. *)
val ellipse : vector -> ?align : [`Corner | `Center] -> vector -> t

(** [circle point radius] is [ellipse point (radius, radius)]. *)
val circle : vector -> ?align : [`Corner | `Center] -> float -> t

(** [arc point size theta1 theta2] is the shape representing the arc (portion
    of a full ellipse) centered at [point] and inscribed in
    [rect point ~align:`Center size] from angle [theta1] to [theta2]. *)
val arc : vector -> ?align : [`Corner | `Center] -> vector ->
  ?stroke_mode : [`Closed | `Open] ->
  ?fill_mode : [`Pie | `Chord] -> ?phi : float -> float -> float -> t

(** [bezier bez] is the shape representing the Bezier curve [bez]. *)
val bezier : Bezier.t -> t

(** {2 Constructing Meta-shapes} *)

(** [group shapes] is the shape that draws [shapes] in order. *)
val group : t list -> t

(** [shape sh tr] is the shape [sh] translated by [tr]. *)
val shape : t -> vector -> t

(** [background color] is the shape that sets the background to [color]. *)
val background : color -> t

(** [empty] is the shape that draws nothing. *)
val empty : t

(** {2 Applying Shape Paints} *)

(** [fill color shape] is [shape] with fill [color], used to draw the inside
    of shapes. *)
val fill : color -> t -> t

(** [no_fill shape] is [shape] with fill cleared. *)
val no_fill : t -> t

(** [stroke color shape] is [shape] with stroke [color], used to draw the
    outline of shapes. *)
val stroke : color -> t -> t

(** [no_stroke shape] is [shape] with stroke cleared. *)
val no_stroke : t -> t

(** [stroke_weight w shape] is [shape] with stroke weight [w], the width of
    the stroke lines. *)
val stroke_weight : float -> t -> t

(** [stroke_cap c shape] is [shape] with stroke cap [c], the way that the ends
    of lines are drawn. *)
val stroke_cap : [`Round | `Square | `Project] -> t -> t

(** [stroke_join j shape] is [shape] with stroke join [j], the way that vertices
    between two lines are drawn. *)
val stroke_join : [`Miter | `Bevel | `Round] -> t -> t

(** [bleach shape] is [shape] with all color (fill or stroke) information
    removed. *)
val bleach : t -> t

(** {2 Names} *)

(** [name n shape] is [shape] tagged with name [n], which can be looked up
    with [find_name]. *)
val name : string -> t -> t

(** [find_name name shape] is [Some found] where [found] is the first shape
    tagged with [name] in [shape], or [None] if [shape] contains no shapes
    tagged [name]. *)
val find_name : string -> t -> t option

(** [find_names name shape] is the list of all shapes in [shape] that are
    tagged with [name]. *)
val find_names : string -> t -> t list

(** {2 Tags} *)

(** [tag tg shape] is [shape] tagged with tag [tg], which can be looked up with
    [find_tag]. Tags are a more general form of names. *)
val tag : tag -> t -> t

(** [find_tag tag shape] is [Some found] where [found] is the first shape tagged
    with [tag] in [shape], or [None] if [shape] contains no shapes tagged
    [tag]. *)
val find_tag : ?eq : (tag -> tag -> bool) -> tag -> t -> t option

(** [find_tags tag shape] is the list of all shapes in [shape] that are tagged
    with [tag]. *)
val find_tags : ?eq : (tag -> tag -> bool) -> tag -> t -> t list

(** {2 Transformations} *)

(** [translate tr shape] is [shape] translated by offset [tr]. *)
val translate : vector -> t -> t

(** [scale (scale_x, scale_y) shape] is [shape] scaled by factors of [scale_x]
    and [scale_y] in the x and y directions, respectively, about the origin.
    To achieve scaling about a different center, first use [translate]. *)
val scale : vector -> t -> t

(** [rotate angle shape] is [shape] rotated [angle] radians about the origin.
    To achieve rotation about a different center, first use [translate]. *)
val rotate : float -> t -> t
