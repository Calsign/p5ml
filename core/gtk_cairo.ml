
open Gtk
open Cairo_gtk
open Cairo

open Color
open Paint
open Config
open Renderer

module rec Gtk_cairo : Renderer = struct
  type buffer =
    {
      window : GWindow.window;
      draw : GMisc.drawing_area;
      painters : (Cairo.context -> unit) Queue.t;
      events : event Queue.t;
      mutex : Mutex.t;
    }

  include BaseRenderer (Gtk_cairo)

  let width buffer = buffer.draw#misc#allocation.Gtk.width
  let height buffer =  buffer.draw#misc#allocation.Gtk.height

  let push_painter buffer painter =
    Queue.push painter buffer.painters

  let queue_event buffer event =
    Queue.push event buffer.events

  let expose buffer draw ev =
    Mutex.lock buffer.mutex;
    let context = Cairo_gtk.create draw#misc#window
    in Seq.iter (fun painter -> painter context) (Queue.to_seq buffer.painters);
    Mutex.unlock buffer.mutex; true

  let redraw_trigger buffer () =
    GtkBase.Widget.queue_draw (buffer.window#as_widget); true

  let event_notifiers =
    [
      `BUTTON_MOTION;
      `BUTTON_PRESS;
      `BUTTON_RELEASE;
      `POINTER_MOTION;
      `KEY_PRESS;
      `KEY_RELEASE;
      `ENTER_NOTIFY;
      `LEAVE_NOTIFY;
      `SCROLL;
    ]

  let handle_mouse_moved buffer event =
    queue_event buffer
      (MouseMoved {x = GdkEvent.Motion.x event |> int_of_float;
                   y = GdkEvent.Motion.y event |> int_of_float}); true

  let build_mouse_coords event =
    {x = GdkEvent.Button.x event |> int_of_float;
      y = GdkEvent.Button.y event |> int_of_float}

  let get_mouse_button event = match GdkEvent.Button.button event with
    | 1 -> `Left
    | 2 -> `Center
    | 3 -> `Right
    | _ -> `Left

  let handle_mouse_pressed buffer event =
    queue_event buffer
      (MousePressed (build_mouse_coords event, get_mouse_button event)); true

  let handle_mouse_released buffer event =
    queue_event buffer
      (MouseReleased (build_mouse_coords event, get_mouse_button event)); true

  let get_key event =
    let key = GdkEvent.Key.keyval event
    in if Uchar.is_valid key then Uchar.of_int key else Uchar.min

  let handle_key_pressed buffer event =
    queue_event buffer
      (KeyPressed (get_key event)); true

  let handle_key_released buffer event =
    queue_event buffer
      (KeyReleased (get_key event)); true

  let handle_window_enter buffer event = true

  let handle_window_leave buffer event = true

  let handle_mouse_scroll buffer event =
    match GdkEvent.Scroll.direction event with
    | `DOWN -> queue_event buffer (MouseScrolled 1); true
    | `UP -> queue_event buffer (MouseScrolled ~-1); true
    | _ -> true

  let handle_window_resized buffer event =
    queue_event buffer
      (WindowResized {width = GdkEvent.Configure.width event;
                      height = GdkEvent.Configure.height event}); false

  let create_buffer frame_rate =
    ignore (GMain.init ());
    let window = GWindow.window ()
    in ignore (window#connect#destroy ~callback:GMain.quit);
    let draw = GMisc.drawing_area ~packing:window#add ()
    in let buffer =
         {
           window = window;
           draw = draw;
           painters = Queue.create ();
           events = Queue.create ();
           mutex = Mutex.create ();
         }
    in ignore (draw#event#connect#expose
                 ~callback:(expose buffer draw));

    window#event#add event_notifiers;
    ignore (window#event#connect#motion_notify
              ~callback:(handle_mouse_moved buffer));
    ignore (window#event#connect#button_press
              ~callback:(handle_mouse_pressed buffer));
    ignore (window#event#connect#button_release
              ~callback:(handle_mouse_released buffer));
    ignore (window#event#connect#key_press
              ~callback:(handle_key_pressed buffer));
    ignore (window#event#connect#key_release
              ~callback:(handle_key_released buffer));
    ignore (window#event#connect#enter_notify
              ~callback:(handle_window_enter buffer));
    ignore (window#event#connect#leave_notify
              ~callback:(handle_window_leave buffer));
    ignore (window#event#connect#scroll
              ~callback:(handle_mouse_scroll buffer));
    ignore (window#event#connect#configure
              ~callback:(handle_window_resized buffer));

    let timeout = 1. /. frame_rate *. 1000.
    in ignore (GMain.Timeout.add
                 (int_of_float timeout) (redraw_trigger buffer));
    window#show ();
    ignore (Thread.create GMain.main ());
    buffer

  let begin_draw buffer =
    Mutex.lock buffer.mutex;
    Queue.clear buffer.painters

  let end_draw buffer =
    Mutex.unlock buffer.mutex

  let clear buffer = ()

  let event_queue buffer =
    let lst = Seq.fold_left (fun acc evt -> evt :: acc) [] (Queue.to_seq buffer.events)
    in Queue.clear buffer.events; lst

  let draw_path_sep stroke_paint fill_paint context =
    let apply_color color = set_source_rgba context
        (float_of_int color.red /. 255.) (float_of_int color.green /. 255.)
        (float_of_int color.blue /. 255.) (float_of_int color.alpha /. 255.) in
    begin
      match fill_paint.fill with
      | Some color ->
        apply_color color;
        fill_preserve context
      | None -> ()
    end;
    begin
      match stroke_paint.stroke with
      | Some color ->
        set_line_cap context
          begin
            match stroke_paint.stroke_cap with
            | `Round -> ROUND
            | `Square -> BUTT
            | `Project -> SQUARE
          end;
        set_line_join context
          begin
            match stroke_paint.stroke_join with
            | `Miter -> JOIN_MITER
            | `Bevel -> JOIN_BEVEL
            | `Round -> JOIN_ROUND
          end;
        set_line_width context (stroke_paint.stroke_weight);
        apply_color color;
        stroke_preserve context
      | None -> ()
    end;
    Path.clear context

  let draw_path paint context = draw_path_sep paint paint context

  let line x1 y1 x2 y2 paint buffer =
    let x1f = float_of_int x1
    in let y1f = float_of_int y1
    in let x2f = float_of_int x2
    in let y2f = float_of_int y2
    in push_painter buffer
      begin fun context ->
        move_to context x1f y1f;
        line_to context x2f y2f;
        draw_path paint context;
      end

  let poly points paint buffer =
    let pointsf = List.map
        (fun (x, y) -> (float_of_int x), (float_of_int y)) points
    in match pointsf with
    | [] -> ()
    | (x, y) :: tl ->
      push_painter buffer
        begin fun context ->
          move_to context x y;
          List.iter (fun (x, y) -> line_to context x y) tl;
          Path.close context;
          draw_path paint context;
        end

  let arc x y ?(align = `Corner) w h ?(stroke_mode = `Open)
      ?(fill_mode = `Pie) rad1 rad2 paint buffer =
    let tw = (float_of_int w) /. 2.
    in let th = (float_of_int h) /. 2.
    in let tx, ty = match align with
        | `Corner -> float_of_int x +. tw, float_of_int y +. th
        | `Center -> float_of_int x, float_of_int y
    in let empty_paint = paint |> no_stroke |> no_fill
    in let initial context =
         save context;
         translate context tx ty;
         scale context tw th;
         arc context 0. 0. 1. rad1 rad2;
         restore context;
    in let path_fill context =
         match fill_mode with
         | `Pie -> line_to context tx ty; Path.close context;
         | `Chord -> Path.close context;
    in push_painter buffer
      begin fun context ->
        (* draw just fill first *)
        initial context;
        path_fill context;
          draw_path_sep empty_paint paint context;
        (* draw just stroke second *)
        initial context;
        begin
          match stroke_mode with
          | `Open -> ();
          | `Closed -> path_fill context;
        end;
        draw_path_sep paint empty_paint context;
      end
end
