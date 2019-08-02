
open Color
open Paint
open Math
open Vector
open Shape
open Renderer

module Graph : Renderer = struct
  type buffer =
    {
      mouse_pressed : bool ref;
      mouse_coords_prev : mouse_coords ref;
      window_dim_prev : (int * int) ref;
      pen_down : vector option ref;
    }

  let create_buffer _ display =
    let width, height = match display with
      | `Size (w, h) -> w, h
      (* Graphics doesn't provide a way to go full screen *)
      | `FullScreen -> 100, 100
    in Graphics.open_graph (Printf.sprintf " %ix%i" width height);
    Graphics.auto_synchronize false;
    {
      mouse_pressed = ref false;
      mouse_coords_prev = ref {x = 0; y = 0};
      window_dim_prev = ref (0, 0);
      pen_down = ref None;
    }

  let begin_draw buffer = buffer.pen_down := None
  let end_draw buffer = Graphics.synchronize ()

  let clear buffer = Graphics.clear_graph ()

  let width buffer = Graphics.size_x ()
  let height buffer = Graphics.size_y ()

  let transform_coords (x, y) = x, height () - y
  let transform_coords_f (x, y) = int_of_float x, height () - (int_of_float y)

  let get_mouse_coords () =
    let x, y = Graphics.mouse_pos () |> transform_coords
    in {x = x; y = y}

  let event_queue buffer =
    let mouse_coords = get_mouse_coords ()
    in let window_dims = width (), height ()
    in let get_mouse_moved tl =
         if mouse_coords = !(buffer.mouse_coords_prev) then tl
         else (buffer.mouse_coords_prev := mouse_coords; MouseMoved mouse_coords :: tl)
    in let get_mouse_events tl =
         match Graphics.button_down (), !(buffer.mouse_pressed) with
         | true, true | false, false -> tl
         | true, false -> buffer.mouse_pressed := true; MousePressed (mouse_coords, `Left) :: tl
         | false, true -> buffer.mouse_pressed := false; MouseReleased (mouse_coords, `Left) :: tl
    in let rec get_key_events tl =
         match Graphics.key_pressed () with
         | true ->
           let key = Graphics.read_key () |> Uchar.of_char
           in get_key_events (KeyReleased key :: KeyPressed key :: tl)
         | false -> tl
    in let get_window_resized tl =
         if window_dims = !(buffer.window_dim_prev) then tl
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

  (* TODO no support for filling *)
  let handle_vertex buffer paint vertex =
    stroke_action paint begin
      fun () ->
        match vertex with
        | MoveTo vec -> buffer.pen_down := Some vec; uncurry Graphics.moveto vec
        | LineTo vec -> uncurry Graphics.lineto vec
        | Arc (center, (dimxf, dimyf), phi, theta1, theta2)
          ->
          begin
            let sx, sy = dimxf *. (cos theta1), dimyf *. (sin theta1)
            in let tx, ty = dimxf *. (cos theta2), dimyf *. (sin theta2)
            in begin
              match !(buffer.pen_down) with
              | None -> buffer.pen_down := Some (center ++ (sx, sy))
              | Some _ -> ()
            end;
            let cenx, ceny = transform_coords_f center
            in let dimx, dimy = int_of_float dimxf, int_of_float dimyf
            in Graphics.draw_arc cenx ceny dimx dimy
              (Math.degrees ~-.theta1 |> int_of_float)
              (Math.degrees ~-.theta2 |> int_of_float);
            uncurry Graphics.moveto (center ++ (tx, ty))
          end
        | BezierTo (vec2, vec3, vec4)
          -> Graphics.curveto
               (transform_coords_f vec2)
               (transform_coords_f vec3)
               (transform_coords_f vec4)
        | ClosePath ->
          begin
            match !(buffer.pen_down) with
            | Some vec -> buffer.pen_down := None; uncurry Graphics.lineto vec
            | None -> ()
          end
    end

  let rec handle_shape buffer paint shape =
    match shape with
    | Shape vertices
      -> List.iter (handle_vertex buffer paint) vertices
    | Group shapes
      -> List.iter (handle_shape buffer paint) shapes
    | Paint (nest_shape, paint_update)
      -> handle_shape buffer (apply_paint_update paint_update paint) nest_shape
    | Name (nest_shape, name)
      -> handle_shape buffer paint nest_shape
    | Background color ->
      begin
        Graphics.set_color (transform_color color);
        Graphics.fill_rect 0 0 (width ()) (height ())
      end
    | Empty -> ()

  let rec render buffer shape =
    handle_shape buffer Paint.create shape
end
