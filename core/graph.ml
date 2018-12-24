
open Color
open Paint
open Renderer

module rec Graph : Renderer = struct
  type buffer = unit

  include BaseRenderer (Graph)

  let create_buffer _ =
    Graphics.open_graph "";
    Graphics.auto_synchronize false

  let begin_draw () = ()
  let end_draw () = Graphics.synchronize ()

  let clear () = Graphics.clear_graph ()

  let width () = Graphics.size_x ()
  let height () = Graphics.size_y ()

  let transform_coords (x, y) = x, height () - y

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
         | true, false -> mouse_pressed := true; MousePressed (mouse_coords, Left) :: tl
         | false, true -> mouse_pressed := false; MouseReleased (mouse_coords, Left) :: tl
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

  let transform_rect (x, y, w, h) =
    let tx, ty = transform_coords (x, y)
    in let cx, cw = if w < 0 then tx + w, -w else tx, w
    in let cy, ch = if h < 0 then ty, -h else ty - h, h
    in cx, cy, cw, ch

  let stroke_action paint action =
    match extr_stroke paint with
    | Some color ->
      begin
        color |> transform_color |> Graphics.set_color;
        extr_stroke_weight paint |> int_of_float |> Graphics.set_line_width;
        action ();
      end
    | None -> ()

  let fill_action paint action =
    match extr_fill paint with
    | Some color ->
      begin
        color |> transform_color |> Graphics.set_color;
        action ();
      end
    | None -> ()

  let line x1 y1 x2 y2 paint () =
    let xc1, yc1 = transform_coords (x1, y1)
    in let xc2, yc2 = transform_coords (x2, y2)
    in begin
      stroke_action paint (fun () ->
          Graphics.moveto xc1 yc1;
          Graphics.lineto xc2 yc2;
        )
    end

  let ellipse x y w h paint () =
    let tx, ty, tw, th = transform_rect (x, y, w, h)
    in let cx, cy, rx, ry = tx + tw / 2, ty + th / 2, tw / 2, th / 2
    in begin
      fill_action paint (fun () -> Graphics.fill_ellipse cx cy rx ry);
      stroke_action paint (fun () -> Graphics.draw_ellipse cx cy rx ry);
    end

  let poly points paint () =
    let arr = List.map transform_coords points |> Array.of_list
    in begin
      fill_action paint (fun () -> Graphics.fill_poly arr);
      stroke_action paint (fun () -> Graphics.draw_poly arr);
    end
end
