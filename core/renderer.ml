
open Paint
open Shape
open Bezier

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
  | WindowClosed

type display =
  [
    | `Size of int * int
    | `FullScreen
  ]

module type Renderer = sig
  type buffer

  val create_buffer : float -> display -> buffer
  val begin_draw : buffer -> unit
  val end_draw : buffer -> unit
  val clear : buffer -> unit

  val event_queue : buffer -> event list

  val width : buffer -> int
  val height : buffer -> int

  val render : buffer -> Shape.t -> unit
end
