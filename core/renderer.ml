
open Paint

type mouse_coords = {x : int; y : int}

type event =
  | MousePressed of mouse_coords * Config.mouse_button
  | MouseDragged of mouse_coords * Config.mouse_button
  | MouseReleased of mouse_coords * Config.mouse_button
  | MouseScrolled of int
  | KeyPressed of char
  | KeyReleased of char
  | WindowResized of {width : int; height : int}

module type Renderer = sig
  type buffer
  type painter

  val create_painter : (paint -> buffer -> unit) -> painter
  val paint : buffer -> paint -> painter -> unit

  val create_buffer : unit -> buffer
  val begin_draw : buffer -> unit
  val end_draw : buffer -> unit
  val clear : buffer -> unit

  val event_queue : buffer -> event list

  val width : buffer -> int
  val height : buffer -> int

  val point : int -> int -> painter
  val line : int -> int -> int -> int -> painter
  val poly : (int * int) list -> painter
  val ellipse : int -> int -> int -> int -> painter
end

module BaseRenderer (R : Renderer) = struct
  type painter = paint -> R.buffer -> unit
  let create_painter func = func
  let paint buffer paint painter = painter paint buffer
end
