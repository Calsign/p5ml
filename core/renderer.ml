(** [Renderer] contains the renderer interface and types for events
   and display settings. *)

open Paint
open Shape
open Bezier

(** A mouse position, with origin in the top-left corner. *)
type mouse_coords = {x : int; y : int}

(** A renderer event. *)
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

(** Display configuration. *)
type display =
  [
    | `Size of int * int
    | `FullScreen
  ]

(** The interface of a renderer. *)
module type Renderer = sig
  (** An abstract handle to a buffer containing all information needed
     to operate the renderer. *)
  type buffer

  (** [create_buffer frame_rate display] is a new buffer with the
     specified frame rate and display settings. *)
  val create_buffer : float -> display -> buffer

  (** [begin_draw ()] is called before [draw] begins. *)
  val begin_draw : buffer -> unit

  (** [end_draw ()] is called after [draw] ends. *)
  val end_draw : buffer -> unit

  (** [clear buffer] removes all rendered shapes from [buffer]. *)
  val clear : buffer -> unit

  (** [event_queue buffer] retrieves and removes the list of queued
     events for [buffer]. *)
  val event_queue : buffer -> event list

  (** [width buffer] is the width of [buffer]. *)
  val width : buffer -> int

  (** [height buffer] is the height of [buffer]. *)
  val height : buffer -> int

  (** [render buffer shape] draws [shape] to [buffer]. *)
  val render : buffer -> Shape.t -> unit
end
