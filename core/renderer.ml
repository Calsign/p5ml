
open Paint
open Bezier
open Shape

type mouse_coords = {x : int; y : int}

type event =
  | MouseMoved of mouse_coords
  | MousePressed of mouse_coords * [`Left | `Right | `Center]
  | MouseReleased of mouse_coords * [`Left | `Right | `Center]
  | MouseScrolled of int
  | MouseEntered
  | MouseExited
  | KeyPressed of Uchar.t
  | KeyReleased of Uchar.t
  | WindowResized of {width : int; height : int}

module type Renderer = sig
  type buffer
  type painter

  val create_painter : (paint -> buffer -> unit) -> painter
  val paint : buffer -> paint -> painter -> unit
  val comp : painter list -> painter

  val create_buffer : float -> buffer
  val begin_draw : buffer -> unit
  val end_draw : buffer -> unit
  val clear : buffer -> unit

  val event_queue : buffer -> event list

  val width : buffer -> int
  val height : buffer -> int

  val line : int -> int -> int -> int -> painter
  val poly : (int * int) list -> painter
  val arc : int -> int -> ?align : [`Corner | `Center] -> int -> int ->
    ?stroke_mode:[`Closed | `Open] ->
    ?fill_mode:[`Pie | `Chord] -> float -> float -> painter
  val bezier : Bezier.t -> painter

  val shape : Shape.t -> painter
end

module BaseRenderer (R : Renderer) = struct
  type painter = paint -> R.buffer -> unit
  let create_painter func = func
  let paint buffer paint painter = painter paint buffer
  let comp painters paint buffer =
    List.iter (fun painter -> painter paint buffer) painters
end
