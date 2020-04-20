
open Config
open Shape

module Handler (S : Sketch.Sketch) : sig
  type hook =
    | EMousePressed
    | EMouseReleased

  type event = {
    hook : hook;
    shape : Shape.t;
    config : config;
    state : S.state;
  }

  type callback = event -> S.state

  type tag += THandler of hook list * callback

  val attach : hook -> callback -> Shape.t -> Shape.t

  val attach_all : hook list -> callback -> Shape.t -> Shape.t
end
