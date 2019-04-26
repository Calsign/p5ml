
open Config
open Shape
open Renderer

module type Sketch = sig
  type state

  module R : Renderer

  val setup : config -> state
  val loop : config -> state -> state
  val draw : config -> state -> Shape.t

  val mouse_pressed : config -> state -> state
  val mouse_released : config -> state -> state
  val mouse_moved : config -> state -> state
  val mouse_dragged : config -> state -> state
  val mouse_clicked : config -> state -> state
  val mouse_scrolled : config -> state -> state

  val key_pressed : config -> state -> state
  val key_released : config -> state -> state
  val key_typed : config -> state -> state

  val window_resized : config -> state -> state
  val window_closed : config -> state -> state
end

module Base (R : Renderer) = struct
  module R = R

  let loop _ st = st
  let draw _ st _ _ = ()

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
