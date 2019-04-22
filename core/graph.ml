
open Color
open Paint
open Math
open Shape
open Renderer

module rec Graph : Renderer = struct
  type buffer = unit

  let create_buffer _ =
    Graphics.open_graph "";
    Graphics.auto_synchronize false

  let begin_draw () = ()
  let end_draw () = Graphics.synchronize ()

  let clear () = Graphics.clear_graph ()

  let width () = Graphics.size_x ()
  let height () = Graphics.size_y ()

  let transform_coords (x, y) = x, height () - y
  let transform_coords_f (x, y) = int_of_float x, height () - (int_of_float y)

  let mouse_pressed = ref false
  let mouse_coords_prev = ref {x = 0; y = 0}
  let window_dim_prev = ref (0, 0)

  let get_mouse_coords () =
    let x, y = Graphics.mouse_pos () |> transform_coords
    in {x = x; y = y}

  let event_queue () =
    let mouse_coords = get_mouse_coords ()
    in let window_dims = width (), height ()
    in let get_mouse_moved tl =
         if mouse_coords = !mouse_coords_prev then tl
         else (mouse_coords_prev := mouse_coords; MouseMoved mouse_coords :: tl)
    in let get_mouse_events tl =
         match Graphics.button_down (), !mouse_pressed with
         | true, true | false, false -> tl
         | true, false -> mouse_pressed := true; MousePressed (mouse_coords, `Left) :: tl
         | false, true -> mouse_pressed := false; MouseReleased (mouse_coords, `Left) :: tl
    in let rec get_key_events tl =
         match Graphics.key_pressed () with
         | true ->
           let key = Graphics.read_key () |> Uchar.of_char
           in get_key_events (KeyReleased key :: KeyPressed key :: tl)
         | false -> tl
    in let get_window_resized tl =
         if window_dims = !window_dim_prev then tl
         else (WindowResized {width = fst window_dims; height = snd window_dims}) :: tl
    in [] |> get_mouse_moved |> get_mouse_events |> get_key_events |> get_window_resized |> List.rev

  let transform_color color = Graphics.rgb color.red color.green color.blue

  let stroke_action paint action =
    match paint.stroke with
    | Some color ->
      begin
        color |> transform_color |> Graphics.set_color;
        paint.stroke_weight |> int_of_float |> Graphics.set_line_width;
        action ();
      end
    | None -> ()

  let fill_action paint action =
    match paint.fill with
    | Some color ->
      begin
        color |> transform_color |> Graphics.set_color;
        action ();
      end
    | None -> ()

  let uncurry func (xf, yf) =
    let x, y = transform_coords_f (xf, yf)
    in func x y

  let handle_vertex paint vertex =
    stroke_action paint begin
      fun () ->
        match vertex with
        | MoveTo vec -> uncurry Graphics.moveto vec
        | LineTo vec -> uncurry Graphics.lineto vec
        | Arc (center, dim, theta1, theta2)
          -> uncurry (uncurry Graphics.draw_arc center) dim
               (Math.degrees theta1 |> int_of_float)
               (Math.degrees theta2 |> int_of_float)
        | BezierTo (vec2, vec3, vec4)
          -> Graphics.curveto
               (transform_coords_f vec2)
               (transform_coords_f vec3)
               (transform_coords_f vec4)
        | ClosePath -> ()
    end

  let rec handle_shape paint shape =
    match shape with
    | Shape vertices
      -> List.iter (handle_vertex paint) vertices
    | Group shapes
      -> List.iter (handle_shape paint) shapes
    | Paint (nest_shape, paint_update)
      -> handle_shape (apply_paint_update paint_update paint) nest_shape
    | Name (nest_shape, name)
      -> handle_shape paint nest_shape
    | Empty -> ()

  let rec render _ shape =
    handle_shape Paint.create shape
end
