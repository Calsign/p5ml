
open Gtk
open Cairo_gtk
open Cairo

open Color
open Paint
open Renderer

module rec Gtk_cairo : Renderer = struct
  type buffer =
    {
      window : GWindow.window;
      draw : GMisc.drawing_area;
      painters : (Cairo.context -> unit) Queue.t;
      mutex : Mutex.t;
    }

  include BaseRenderer (Gtk_cairo)

  let width buffer = buffer.draw#misc#allocation.Gtk.width
  let height buffer =  buffer.draw#misc#allocation.Gtk.height

  let push_painter buffer painter =
    Queue.push painter buffer.painters

  let expose buffer draw ev =
    Mutex.lock buffer.mutex;
    let context = Cairo_gtk.create draw#misc#window
    in Seq.iter (fun painter -> painter context) (Queue.to_seq buffer.painters);
    Mutex.unlock buffer.mutex; true

  let redraw_trigger buffer () =
    GtkBase.Widget.queue_draw (buffer.window#as_widget); true

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
           mutex = Mutex.create ();
         }
    in ignore (draw#event#connect#expose
                 ~callback:(expose buffer draw));
    let timeout = 1. /. frame_rate *. 1000.
    in ignore (GMain.Timeout.add
                 (int_of_float timeout) (redraw_trigger buffer));
    window#show (); ignore (Thread.create GMain.main ()); buffer

  let begin_draw buffer =
    Mutex.lock buffer.mutex;
    Queue.clear buffer.painters

  let end_draw buffer =
    Mutex.unlock buffer.mutex

  let clear buffer = ()

  let event_queue buffer =
    (* todo events *)
    WindowResized {width = width buffer; height = height buffer} :: []

  let draw_path paint context =
    let apply_color color = set_source_rgba context
        (float_of_int color.red /. 255.) (float_of_int color.green /. 255.)
        (float_of_int color.blue /. 255.) (float_of_int color.alpha /. 255.) in
    begin
      match extr_fill paint with
      | Some color -> apply_color color; fill_preserve context
      | None -> ()
    end;
    begin
      match extr_stroke paint with
      | Some color -> apply_color color; stroke_preserve context
      | None -> ()
    end;
    Path.clear context

  let point x y paint buffer = ()

  let line x1 y1 x2 y2 paint buffer = ()

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
          draw_path paint context
        end

  let ellipse x y w h paint buffer = ()
end
