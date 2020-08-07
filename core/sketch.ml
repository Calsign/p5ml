(** [Sketch] contains the sketch interface and the base implementation. *)

open Config
open Shape
open Renderer

(** The interface of a sketch. *)
module type Sketch = sig
  (** The type of the state, used for all information stored between
     frames. *)
  type state

  module R : Renderer

  (** The type of the display, e.g. [`Size (400, 400)]. *)
  val display : display

  (** {2 Main Functions} *)

  (** [setup config] is the initial state. *)
  val setup : config -> state

  (** [loop config state] is the state that results after advancing
     [state] through one time step. *)
  val loop : config -> state -> state

  (** [draw config state] is the shape that should be drawn in the
     given [state]. *)
  val draw : config -> state -> Shape.t

  (** {2 Mouse Event Functions} *)

  (** [mouse_pressed config state] is the state that results after the
     mouse is pressed in [state]. Mouse information can be obtained
     from [config]. *)
  val mouse_pressed : config -> state -> state

  (** [mouse_released config state] is the state results after the
     mouse is released in [state]. Mouse information can be obtained
     from [config]. *)
  val mouse_released : config -> state -> state

  (** [mouse_moved config state] is the state that results after the
     mouse is moved in [state]. Mouse information can be obtained from
     [config]. *)
  val mouse_moved : config -> state -> state

  (** [mouse_dragged config state] is the state that results after the
     mouse is dragged (moved while pressed) in [state]. Mouse
     information can be obtained from [config]. *)
  val mouse_dragged : config -> state -> state

  (** [mouse_clicked config state] is the state that results after the
     mouse is clicked (pressed and then released) in [state]. Mouse
     information can be obtained from [config]. *)
  val mouse_clicked : config -> state -> state

  (** [mouse_scrolled config state] is the state that results after
     the mouse is scrolled in [state]. Mouse information can be
     obtained from [config]. *)
  val mouse_scrolled : config -> state -> state

  (** {2 Key Event Functions} *)

  (** [key_pressed config state] is the state that results after a key
     is pressed in [state]. Key information can be obtained from
     [config]. *)
  val key_pressed : config -> state -> state

  (** [key_released config state] is the state that results after a
     key is released in [state]. Key information can be obtained from
     [config]. *)
  val key_released : config -> state -> state

  (** [key_typed config state] is the state that results after a key
     is typed (pressed and released) in [state]. Key information can
     be obtained from [config]. *)
  val key_typed : config -> state -> state

  (** {2 Window State Event Functions} *)

  (** [window_resized config state] is the state that results after
     the window is resized in [state]. Window information can be
     obtained from [config]. *)
  val window_resized : config -> state -> state

  (** [window_closed config state] is the state that results after the
     window is closed in [state]. Window information can be obtained
     from [config]. The default action is to raise [Exit]. *)
  val window_closed : config -> state -> state
end

(** [Base] contains default implementations of most sketch
   functions. It does not provide the type of [state] or the function
   [setup]; these must be specified for every sketch. *)
module Base (R : Renderer) = struct
  module R = R

  let display = `Size (100, 100)

  let loop _ st = st
  let draw _ _ = group []

  let mouse_pressed _ st = st
  let mouse_released _ st = st
  let mouse_moved _ st = st
  let mouse_dragged _ st = st
  let mouse_clicked _ st = st
  let mouse_scrolled _ st = st

  let key_pressed _ st = st
  let key_released _ st = st
  let key_typed _ st = st

  let window_resized _ st = st
  let window_closed _ _ = raise Exit
end
