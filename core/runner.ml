(** [Runner] contains code for running sketches. *)

open Config
open Shape
open Sketch
open Renderer
open Key

(** Used internally by dynamic runner *)
module Dynamic : sig
  val set_dynamic_handler : (unit -> unit) -> unit
  val apply_handler : unit -> unit
  val is_dynamic : unit -> bool
  val set_initialized : unit -> unit
  val is_initialized : unit -> bool
  val set_dynamic_hotswap : (Obj.t -> config -> Obj.t -> unit) -> unit
  val apply_dynamic_hotswap : 'a -> config -> 'b -> unit
  val has_dynamic_hotswap : unit -> bool
  val set_change_notifier : (unit -> bool) -> unit
  val has_changed : unit -> bool
end = struct
  let handler : (unit -> unit) option ref = ref None
      
  let set_dynamic_handler v = handler := Some v

  let apply_handler () = match !handler with
    | Some func -> func ()
    | None -> ()

  let is_dynamic () = match !handler with
    | Some _ -> true
    | None -> false

  let init = ref false
  let set_initialized () = init := true
  let is_initialized () = !init

  let dynamic_hotswap : (Obj.t -> config -> Obj.t -> unit) option ref = ref None

  let set_dynamic_hotswap func = dynamic_hotswap := Some func

  let apply_dynamic_hotswap buffer_mag config state_mag =
    match !dynamic_hotswap with
    | Some func ->
      begin
        dynamic_hotswap := None;
        func (Obj.repr buffer_mag) config (Obj.repr state_mag)
      end
    | None -> failwith "Tried to perform dynamic hot-swap without caching first"

  let has_dynamic_hotswap () =
    match !dynamic_hotswap with
    | Some _ -> true
    | None -> false

  let change_notifier : (unit -> bool) option ref = ref None
  let set_change_notifier func = change_notifier := Some func
  let has_changed () = match !change_notifier with
    | Some func -> func ()
    | None -> false
end

module Runner (S : Sketch) : sig
  val run : unit -> unit
  val load_dynamic_module : Obj.t -> config -> Obj.t -> unit
end = struct
  let target_frame_rate = 60.

  let default_background_color = Color.gray 127

  module KeysSet = Set.Make (Uchar)

  let create_config buffer =
    {
      width = S.R.width buffer;
      height = S.R.height buffer;
      display_width = 0;
      display_height = 0;

      mouse_x = 0;
      mouse_y = 0;
      pmouse_x = 0;
      pmouse_y = 0;
      mouse_scroll = 0;
      mouse_pressed = false;
      mouse_button = `Left;

      key = Char.chr 0;
      keys = KeysSet.empty;
      key_unicode = Uchar.min;
      key_pressed = false;

      frame_count = 0;
      frame_rate = target_frame_rate
    }

  let loop_config config =
    {
      config with
      pmouse_x = config.mouse_x;
      pmouse_y = config.mouse_y;
      mouse_scroll = 0;
      frame_count = config.frame_count + 1;
    }

  let update_config_mouse config x y button pressed =
    {
      config with
      mouse_x = x;
      mouse_y = y;
      mouse_pressed = pressed;
      mouse_button = button;
    }

  open Key
  open KeyUnicode

  let unicode_key_mappings =
    [
      (KeyUnicode.backspace, Key.backspace);
      (KeyUnicode.tab, Key.tab);
      (KeyUnicode.enter, Key.enter);
      (KeyUnicode.return, Key.return);
      (KeyUnicode.esc, Key.esc);
      (KeyUnicode.delete, Key.delete);
    ]

  let char_of_unicode unicode =
    if Uchar.is_char unicode then Uchar.to_char unicode
    else match List.assoc_opt unicode unicode_key_mappings with
      | Some key -> key
      | None -> '\000'

  let handle_event (config, state) = function
    | MousePressed ({x; y}, button) ->
      let config' = update_config_mouse config x y button true
      in config', S.mouse_pressed config' state
    | MouseMoved {x; y} ->
      let config' = update_config_mouse config x y config.mouse_button true
      in config', S.mouse_moved config
           begin
             if config.mouse_pressed
             then S.mouse_dragged config' state
             else state
           end
    | MouseReleased ({x; y}, button) ->
      let config' = update_config_mouse config x y button false
      in config', S.mouse_released config' state |> S.mouse_clicked config'
    | MouseScrolled scroll ->
      let config' = {config with mouse_scroll = scroll}
      in config', S.mouse_scrolled config' state
    | MouseEntered -> config, state
    | MouseExited -> config, state
    | KeyPressed unicode ->
      let key = char_of_unicode unicode
      in if key = Key.esc then raise Exit
      else let config' =
             {config with key_pressed = true;
                          key = key; key_unicode = unicode;
                          keys = KeysSet.add unicode config.keys}
        in config', S.key_pressed config' state
    | KeyReleased unicode ->
      let key = char_of_unicode unicode
      in let keys' = KeysSet.remove unicode config.keys
      in let config' = {config with key_pressed = not (KeysSet.is_empty keys');
                                    key = key; key_unicode = unicode;
                                    keys = keys'}
      in config', S.key_released config' state |> S.key_typed config'
    | WindowResized {width; height} ->
      let config' = {config with width = width; height = height}
      in config', S.window_resized config' state
    | WindowClosed -> config, S.window_closed config state

  let default_background config buffer =
    Shape.background default_background_color

  let rec loop buffer config state prev_frame_time =
    let frame_time = Unix.gettimeofday ()
    in let config' = loop_config config
    in let config'', state' =
         List.fold_left handle_event (config', state) (S.R.event_queue buffer)
    in let state'' = S.loop config' state'
    in let painter = Shape.group [
        default_background config'' buffer;
        S.draw config'' state'';
      ]
    in begin
      S.R.begin_draw buffer;
      S.R.clear buffer;
      S.R.render buffer painter;
      S.R.end_draw buffer;
      (* how much longer should we wait in order to achieve our target frame rate?
         0.005 second buffer is to give CPU a breath *)
      let sleep_time = max 0.005 (frame_time +. 1. /. target_frame_rate -. (Unix.gettimeofday ()))
      (* this should be exactly target_frame_rate unless the frame rate drops
         because the sketch is doing really intense work; we need to add in
         sleep_time because if the frame rate drops, we still have the 0.005
         second buffer *)
      in let config''' = {config'' with frame_rate = 1. /. (frame_time -. prev_frame_time)}
      in Unix.sleepf sleep_time;
      (* TODO: maybe don't need to ping inotify at 60fps *)
      if Dynamic.is_dynamic () && Dynamic.has_changed ()
      then Dynamic.apply_handler ()
      else ();
      if Dynamic.is_dynamic () && Dynamic.has_dynamic_hotswap ()
      then Dynamic.apply_dynamic_hotswap buffer config'' state''
      else loop buffer config''' state'' frame_time
    end

  let wrap_handle_exns func =
    try func () with
    | Exit -> ()

  let run () =
    wrap_handle_exns
      begin fun () ->
        let buffer = S.R.create_buffer target_frame_rate S.display
        in let config = create_config buffer
        in let state = S.setup config
        in loop buffer config state (Unix.gettimeofday ())
      end

  let load_dynamic_module buffer_mag config state_mag =
    wrap_handle_exns
      begin fun () ->
        (* do not try this at home kids *)
        loop (Obj.obj buffer_mag) config (Obj.obj state_mag) (Unix.gettimeofday ())
      end
end

(** [run_sketch (module MySketch)] runs the sketch in the module
   [MySketch]. *)
let run_sketch (sketch : (module Sketch)) : unit =
  let module Run = Runner (val sketch : Sketch) in
  begin
    if Dynamic.is_initialized ()
    then Dynamic.set_dynamic_hotswap Run.load_dynamic_module
    else (Dynamic.set_initialized (); Run.run ())
  end
