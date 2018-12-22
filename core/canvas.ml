
open Color
open Paint
open Renderer

module Canvas (R : Renderer) : sig
  val comp : R.painter list -> R.painter

  val background : color -> R.painter
  val point : int -> int -> R.painter
  val line : int -> int -> int -> int -> R.painter
  val rect : int -> int -> int -> int -> R.painter
  val triangle : int -> int -> int -> int -> int -> int -> R.painter
  val quad : int -> int -> int -> int -> int -> int -> int -> int -> R.painter
  val poly : (int * int) list -> R.painter
  val ellipse : int -> int -> int -> int -> R.painter

  val fill : color -> R.painter -> R.painter
  val stroke : color -> R.painter -> R.painter
  val no_fill : R.painter -> R.painter
  val no_stroke : R.painter -> R.painter
  val stroke_weight : float -> R.painter -> R.painter
end = struct
  let comp painters paint buffer = List.iter (fun painter -> painter paint buffer) painters

  let fill color painter paint = painter (Paint.fill color paint)
  let stroke color painter paint = painter (Paint.stroke color paint)
  let no_fill painter paint = painter (Paint.no_fill paint)
  let no_stroke painter paint = painter (Paint.no_stroke paint)
  let stroke_weight weight painter paint = painter (Paint.stroke_weight weight paint)

  let point = R.point
  let line = R.line
  let poly = R.poly
  let ellipse = R.ellipse

  let rect x y w h = poly [(x, y); (x+w, y); (x+w, y+h); (x, y+h)]
  let triangle x1 y1 x2 y2 x3 y3 = poly [(x1, y1); (x2, y2); (x3, y3)]
  let quad x1 y1 x2 y2 x3 y3 x4 y4 = poly [(x1, y1); (x2, y2); (x3, y3); (x4, y4)]

  let background color paint buffer = comp [rect 0 0 (R.width buffer) (R.height buffer) |> fill color] paint buffer
end
