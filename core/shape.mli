
open Math

module Shape : sig
  type vector = Vector.t

  type vertex = private
    | MoveTo of vector
    | LineTo of vector
    | Bezier of vector * vector * vector
  and t = private
    | Group of t list
    | Shape of {vertices : vertex list; contour : t option}

  val create :
    ?kind :
      [
        | `Shape
        | `Points
        | `Lines
        | `Triangles
        | `Triangle_strip
        | `Triangle_fan
        | `Quads
        | `Quad_strip
      ] ->
    ?cap : [`Open | `Closed] ->
    ?contour : t ->
    [
      | `Vertex of vector
      | `Bezier of vector * vector * vector
    ] list -> t

  val group : t list -> t

  val vert : int -> int ->
    [
      | `Vertex of vector
      | `Bezier of vector * vector * vector
    ]

  val vert_bezier : int -> int -> int -> int -> int -> int ->
    [
      | `Vertex of vector
      | `Bezier of vector * vector * vector
    ]
end
