
open Color
open Paint
open Math
open Vector

type vector = Vector.t

type vertex =
  | MoveTo of vector
  | LineTo of vector
  | BezierTo of vector * vector * vector
  | Arc of vector * vector * float * float * float
  | ClosePath

type t =
  | Shape of vertex list
  | Group of t list
  | Paint of t * paint_update
  | Name of t * string
  | Background of color
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
    ?(stroke_mode=`Closed) ?(fill_mode=`Chord) ?(phi=0.) theta1 theta2 =
  let cx, cy = match align with
    | `Corner -> x +. w /. 2., y +. h /. 2.
    | `Center -> x, y
  in let base = Arc ((cx, cy), (w, h) // 2., phi, theta1, theta2)
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

let background color = Background color

let empty = Empty

let name name shape =
  Name (shape, name)

let rec find_named name shape =
  match shape with
  | Shape _ -> None
  | Group shapes ->
    begin
      match List.map (find_named name) shapes with
      | hd :: _ -> hd
      | _ -> None
    end
  | Paint (nest_shape, _) -> find_named name nest_shape
  | Name (nest_shape, nest_name) ->
    if name = nest_name then Some nest_shape
    else find_named name nest_shape
  | Background _ -> None
  | Empty -> None

let rec find_all_named name shape =
  match shape with
  | Shape _ -> []
  | Group shapes ->
    begin
      List.fold_left (@) [] (List.map (find_all_named name) shapes)
    end
  | Paint (nest_shape, _) -> find_all_named name nest_shape
  | Name (nest_shape, nest_name) ->
    if name = nest_name then [nest_shape]
    else find_all_named name nest_shape
  | Background _ -> []
  | Empty -> []

let transform_vertex angle (scale_x, scale_y, scale_mag)
    (func : vector -> vector) (vertex : vertex) : vertex =
  match vertex with
  | MoveTo vector -> MoveTo (func vector)
  | LineTo vector -> LineTo (func vector)
  | BezierTo (vec2, vec3, vec4) -> BezierTo (func vec2, func vec3, func vec4)
  | Arc (center, (dim_x, dim_y), phi, theta1, theta2) ->
    let scaled_dim = dim_x *. scale_x, dim_y *. scale_y
    in Arc (func center, scaled_dim, Math.angle_sum phi angle, theta1, theta2)
  | ClosePath -> ClosePath

let rec transform_shape (angle : float) (scales : float * float * float)
    (func : vector -> vector) (shape : t) : t =
  match shape with
  | Shape vertices -> Shape (List.map (transform_vertex angle scales func) vertices)
  | Group shapes -> Group (List.map (transform_shape angle scales func) shapes)
  | Paint (nest_shape, paint_update) ->
    begin
      (* TODO no handling for inequal x and y scales *)
      let scale_x, scale_y, scale_mag = scales
      in let scaled_paint_update = match paint_update with
        | Stroke_weight nest_scale -> Stroke_weight (nest_scale *. scale_mag)
        | _ -> paint_update
      in Paint (transform_shape angle scales func nest_shape, scaled_paint_update)
    end
  | Name (nest_shape, name) -> Name (transform_shape angle scales func nest_shape, name)
  | Background color -> Background color
  | Empty -> Empty

let translate offset shape =
  transform_shape 0. (1., 1., 1.) (fun vertex -> vertex ++ offset) shape

let scale (scale_x, scale_y) shape =
  let scale_mag = (mag (scale_x, scale_y)) /. (mag (1., 1.))
  in Paint (transform_shape 0. (scale_x, scale_y, scale_mag)
              (fun (x, y) -> x *. scale_x, y *. scale_y) shape,
            Stroke_weight_scale (scale_mag))

let rotate theta shape =
  transform_shape theta (1., 1., 1.) (fun vertex -> rotate vertex theta) shape

let shape vector shape = Group [translate shape vector]
