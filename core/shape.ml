
open Color
open Paint
open Math
open Vector

type vector = Vector.t

type vertex =
  | MoveTo of vector
  | LineTo of vector
  | BezierTo of vector * vector * vector
  | Arc of vector * vector * float * float
  | ClosePath

type t =
  | Shape of vertex list
  | Group of t list
  | Paint of t * paint_update
  | Name of t * string
  | Empty

let fill color shape =
  Paint (shape, Fill (Some color))

let no_fill shape =
  Paint (shape, Fill None)

let stroke color shape =
  Paint (shape, Stroke (Some color))

let no_stroke shape =
  Paint (shape, Stroke None)

let stroke_weight weight shape =
  Paint (shape, Stroke_weight weight)

let stroke_cap cap shape =
  Paint (shape, Stroke_cap cap)

let stroke_join join shape =
  Paint (shape, Stroke_join join)

let poly ?(close = `Close) (vertices : vector list) : t =
  match vertices with
  | [] -> Shape []
  | hd :: _ ->
    Shape
      begin
        begin
          MoveTo hd ::
          begin
            List.fold_left
              (fun acc vert -> LineTo vert :: acc)
              [] vertices |> List.rev
          end
        end @ match close with
        | `Close -> [ClosePath]
        | `Open -> []
      end

let point vertex = poly [vertex] ~close:`Open
let line v1 v2 = poly [v1; v2] ~close:`Open

let rect (x, y) ?(align = `Corner) (w, h) =
  let tx, ty = match align with
    | `Corner -> x, y
    | `Center -> x -. w /. 2., y -. h /. 2.
  in poly [(tx, ty); (tx+.w, ty);
           (tx+.w, ty+.h); (tx, ty+.h)]

let quad v1 v2 v3 v4 = poly [v1; v2; v3; v4]
let triangle v1 v2 v3 = poly [v1; v2; v3]

let arc (x, y) ?(align = `Center) (w, h)
    ?(stroke_mode=`Closed) ?(fill_mode=`Chord) theta1 theta2 =
  let cx, cy = match align with
    | `Corner -> x +. w /. 2., y +. h /. 2.
    | `Center -> x, y
  in let base = Arc ((cx, cy), (w, h) // 2., theta1, theta2)
  in let fill_lst = match fill_mode with
      | `Chord -> [base; ClosePath]
      | `Pie -> [base; LineTo (cx, cy); ClosePath]
  in let stroke_lst = match stroke_mode with
      | `Open -> [base]
      | `Closed ->fill_lst
  in Group [
    Shape fill_lst |> no_stroke;
    Shape stroke_lst |> no_fill;
  ]

let ellipse v1 ?align v2 =
  arc v1 ?align:align v2 0. Math.two_pi

let circle v1 ?align rad =
  ellipse v1 ?align:align (rad, rad)

let bezier (v1, v2, v3, v4) =
  Shape [MoveTo v1; BezierTo (v2, v3, v4)]

let group shapes = Group shapes

let empty = Empty

let name name shape =
  Name (shape, name)
