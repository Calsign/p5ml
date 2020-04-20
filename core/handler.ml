
open Config
open Shape

module Handler (S : Sketch.Sketch) = struct
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

  let attach_all hooks callback shape =
    tag (THandler (hooks, callback)) shape

  let attach hook callback shape =
    attach_all [hook] callback shape
end
