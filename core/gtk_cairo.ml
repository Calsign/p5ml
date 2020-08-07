(** A renderer using GTK+/cairo2. *)

open Gtk
open Cairo_gtk
open Cairo

open Color
open Paint
open Config
open Math
open Shape
open Renderer

(** An implementation of Renderer using GTK+ and cairo2 as the
   backend. This is the main renderer that supports all p5ml
   features. *)
module Gtk_cairo : Renderer = struct
  type buffer =
    {
      window : GWindow.window;
      draw : GMisc.drawing_area;
      shape : Shape.t ref;
      surface : Surface.t ref;
      context : Cairo.context ref;
      events : event Queue.t;
      mutex : Mutex.t;
    }

  let width buffer = buffer.draw#misc#allocation.Gtk.width
  let height buffer =  buffer.draw#misc#allocation.Gtk.height

  let apply_color color context =
    set_source_rgba context
      (float_of_int color.red /. 255.) (float_of_int color.green /. 255.)
      (float_of_int color.blue /. 255.) (float_of_int color.alpha /. 255.)

  let draw_path_sep stroke_paint fill_paint context =
    begin
      match fill_paint.fill with
      | Some color ->
        apply_color color context;
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
        apply_color color context;
        stroke_preserve context
      | None -> ()
    end;
    Path.clear context

  let draw_path paint context = draw_path_sep paint paint context

  let handle_apply_vertex paint context vertex =
    match vertex with
    | MoveTo (x, y) -> move_to context x y
    | LineTo (x, y) -> line_to context x y
    | Arc ((cx, cy), (w, h), phi, theta1, theta2) ->
      begin
        save context;
        Cairo.translate context cx cy;
        Cairo.rotate context phi;
        Cairo.scale context w h;
        Cairo.arc context 0. 0. 1. theta1 theta2;
        restore context
      end
    | BezierTo ((x2, y2), (x3, y3), (x4, y4))
      -> curve_to context x2 y2 x3 y3 x4 y4
    | ClosePath -> Path.close context

  let rec handle_apply_shape paint context shape : unit =
    match shape with
    | Shape vertices ->
      begin
        List.iter (handle_apply_vertex paint context) vertices;
        draw_path paint context
      end
    | Group shapes
      -> List.iter (handle_apply_shape paint context) shapes
    | Paint (nest_shape, paint_update)
      -> handle_apply_shape (apply_paint_update paint_update paint) context nest_shape
    | Tag (nest_shape, tag)
      -> handle_apply_shape paint context nest_shape
    | Background color ->
      begin
        apply_color color context;
        Cairo.paint context
      end
    | Empty -> ()

  let queue_event buffer event =
    Queue.push event buffer.events

  let expose buffer draw ev =
    Mutex.lock buffer.mutex;
    let context = Cairo_gtk.create draw#misc#window in
    (* copy buffer to window; performing actual drawing here is bad *)
    Cairo.set_source_surface context !(buffer.surface) ~x:0. ~y:0.;
    Cairo.paint context;
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
    (* dimensions of existing surface *)
    let surf_width = Cairo.Image.get_width !(buffer.surface) in
    let surf_height = Cairo.Image.get_height !(buffer.surface) in
    (* new dimensions of window *)
    let width = GdkEvent.Configure.width event in
    let height = GdkEvent.Configure.height event in
    (* re-use old surface if it's big enough *)
    if width > surf_width || height > surf_height
    then begin
      (* if we need to resize, then double each dimension
         to minimize number of surface re-allocations necessary *)
      let surf_width' = max width (surf_width * 2) in
      let surf_height' = max height (surf_height * 2) in
      (* create new surface; we must use ARGB, otherwise we get X errors *)
      let surface = Cairo.Image.create ARGB32 surf_width' surf_height' in
      let context = Cairo.create surface in
      Mutex.lock buffer.mutex;
      (* copy old surface over to reduce tearing due to synchronization issues *)
      Cairo.set_source_surface context !(buffer.surface) ~x:0. ~y:0.;
      Cairo.paint context;
      (* switch to new surface *)
      buffer.context := context;
      buffer.surface := surface;
      Mutex.unlock buffer.mutex;
    end;
    (* queue resize event *)
    queue_event buffer
      (WindowResized {width; height}); false

  let handle_window_delete buffer event =
    queue_event buffer WindowClosed; false

  let create_buffer frame_rate display =
    ignore (GMain.init ());
    (* DIALOG makes window open as a pop-up *)
    let window = match display with
      | `Size (width, height) -> GWindow.window ~type_hint:`DIALOG ~width ~height ()
      | `FullScreen -> let w = GWindow.window ~type_hint:`DIALOG () in w#fullscreen (); w
    in ignore (window#connect#destroy ~callback:GMain.quit);
    let draw = GMisc.drawing_area ~packing:window#add ()
    (* create a 100x100 buffer at first; we will receive a resize event immediately *)
    in let surface = Cairo.Image.create ARGB32 100 100;
    in let buffer =
         {
           window = window;
           draw = draw;
           shape = ref Shape.empty;
           surface = ref surface;
           context = ref (Cairo.create surface);
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
    ignore (window#event#connect#delete
              ~callback:(handle_window_delete buffer));

    let timeout = 1. /. frame_rate *. 1000.
    in ignore (GMain.Timeout.add
                 (int_of_float timeout) (redraw_trigger buffer));
    window#show ();
    ignore (Thread.create GMain.main ());
    buffer

  let begin_draw buffer = ()

  let end_draw buffer = ()

  (** [clear buffer] does not modify the buffer. This is because
      the Cairo surface gets cleared every frame automatically. *)
  let clear buffer = ()

  let event_queue buffer =
    let lst = Queue.fold (fun acc evt -> evt :: acc) [] buffer.events
    in Queue.clear buffer.events; List.rev lst

  let render buffer shape =
    Mutex.lock buffer.mutex;
    buffer.shape := shape;
    (* erase existing content *)
    Cairo.set_source_rgb !(buffer.context) 0. 0. 0.;
    Cairo.paint !(buffer.context);
    (* draw the new shape *)
    handle_apply_shape Paint.create !(buffer.context) shape;
    Mutex.unlock buffer.mutex
end
