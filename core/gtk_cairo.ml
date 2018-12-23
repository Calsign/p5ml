
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
      record : Surface.t;
      context : Cairo.context;
      mutex : Mutex.t;
    }

  include BaseRenderer (Gtk_cairo)

  let width buffer = buffer.draw#misc#allocation.Gtk.width
  let height buffer =  buffer.draw#misc#allocation.Gtk.height

  let expose buffer draw ev =
    Mutex.lock buffer.mutex;
    let context = Cairo_gtk.create draw#misc#window
    in set_source_surface context buffer.record 0. 0.;
    rectangle context 0. 0. (width buffer |> float_of_int) (height buffer |> float_of_int);
    Cairo.fill context;
    Mutex.unlock buffer.mutex; true

  let create_buffer () =
    ignore (GMain.init ());
    let window = GWindow.window ()
    in ignore (window#connect#destroy ~callback:GMain.quit);
    let draw = GMisc.drawing_area ~packing:window#add ()
    in let surface = Cairo.Recording.create Cairo.COLOR_ALPHA
    in let context = Cairo.create surface
    in let buffer =
         {
           window = window;
           draw = draw;
           record = surface;
           context = context;
           mutex = Mutex.create ();
         }
    in ignore (draw#event#connect#expose
                 ~callback:(expose buffer draw));
    ignore (GMain.Timeout.add (int_of_float ((1. /. 60.) *. 1000.)) (fun () -> GtkBase.Widget.queue_draw (buffer.window#as_widget); true));
    window#show (); ignore (Thread.create GMain.main ()); buffer

  let begin_draw buffer = Mutex.lock buffer.mutex
  let end_draw buffer = Mutex.unlock buffer.mutex
  let clear buffer = ()

  let event_queue buffer = WindowResized {width = width buffer; height = height buffer} :: []

  let draw_path paint buffer =
    let apply_color color = set_source_rgba buffer.context
        (float_of_int color.red /. 255.) (float_of_int color.green /. 255.)
        (float_of_int color.blue /. 255.) (float_of_int color.alpha /. 255.) in
    begin
      match extr_fill paint with
      | Some color -> apply_color color; fill_preserve buffer.context
      | None -> ()
    end;
    begin
      match extr_stroke paint with
      | Some color -> apply_color color; stroke_preserve buffer.context
      | None -> ()
    end;
    Path.clear buffer.context

  let point x y paint buffer = ()

  let line x1 y1 x2 y2 paint buffer = ()

  let poly points paint buffer =
    let pointsf = List.map
        (fun (x, y) -> (float_of_int x), (float_of_int y)) points
    in match pointsf with
    | [] -> ()
    | (x, y) :: tl ->
      move_to buffer.context x y;
      List.iter (fun (x, y) -> line_to buffer.context x y) tl;
      Path.close buffer.context;
      draw_path paint buffer

  let ellipse x y w h paint buffer = ()
end
