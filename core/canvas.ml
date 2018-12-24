
open Color
open Paint
open Math
open Bezier
open Renderer

module Canvas (R : Renderer) : sig
  val comp : R.painter list -> R.painter

  val background : color -> R.painter
  val point : int -> int -> R.painter
  val line : int -> int -> int -> int -> R.painter
  val rect : int -> int -> ?align : [`Corner | `Center] -> int -> int -> R.painter
  val triangle : int -> int -> int -> int -> int -> int -> R.painter
  val quad : int -> int -> int -> int -> int -> int -> int -> int -> R.painter
  val poly : (int * int) list -> R.painter
  val ellipse : int -> int -> ?align : [`Corner | `Center] -> int -> int -> R.painter
  val arc : int -> int -> ?align : [`Corner | `Center] -> int -> int ->
    ?stroke_mode:[`Closed | `Open] ->
    ?fill_mode:[`Pie | `Chord] -> float -> float -> R.painter
  val bezier : Bezier.t -> R.painter

  val fill : color -> R.painter -> R.painter
  val stroke : color -> R.painter -> R.painter
  val no_fill : R.painter -> R.painter
  val no_stroke : R.painter -> R.painter
  val stroke_weight : float -> R.painter -> R.painter
  val stroke_cap : [`Round | `Square | `Project] -> R.painter -> R.painter
  val stroke_join : [`Miter | `Bevel | `Round] -> R.painter -> R.painter
end = struct
  let comp = R.comp

  let create_paint_mutator mutator painter =
    R.create_painter (fun paint buffer -> R.paint buffer (mutator paint) painter)

  let fill color = Paint.fill color |> create_paint_mutator
  let stroke color = Paint.stroke color |> create_paint_mutator
  let no_fill = Paint.no_fill |> create_paint_mutator
  let no_stroke = Paint.no_stroke |> create_paint_mutator
  let stroke_weight weight = Paint.stroke_weight weight |> create_paint_mutator
  let stroke_cap cap = Paint.stroke_cap cap |> create_paint_mutator
  let stroke_join join = Paint.stroke_join join |> create_paint_mutator

  let line = R.line
  let poly = R.poly
  let arc = R.arc
  let bezier = R.bezier

  let rect x y ?(align = `Corner) w h =
    let tx, ty = match align with
      | `Corner -> x, y
      | `Center -> x - w / 2, y - h / 2
    in poly [(tx, ty); (tx+w, ty); (tx+w, ty+h); (tx, ty+h)]

  let triangle x1 y1 x2 y2 x3 y3 = poly [(x1, y1); (x2, y2); (x3, y3)]
  let quad x1 y1 x2 y2 x3 y3 x4 y4 = poly [(x1, y1); (x2, y2); (x3, y3); (x4, y4)]
  let ellipse x y ?(align = `Corner) w h = arc x y ~align:align w h 0. Math.two_pi

  (** [point x y] internally uses [ellipse]. *)
  let point x y = R.create_painter
      begin fun paint buffer ->
        match paint.stroke with
        | Some color ->
          begin
            let paint' = paint |> Paint.no_stroke |> Paint.fill color
            in let dim = paint.stroke_weight |> int_of_float
            in R.paint buffer paint' (ellipse (x - dim / 2) (y - dim / 2) dim dim)
          end
        | None -> ()
      end

  let background color = R.create_painter
      (fun paint buffer -> R.paint buffer paint
          (comp [rect 0 0 (R.width buffer) (R.height buffer) |> fill color |> stroke color]))
end
