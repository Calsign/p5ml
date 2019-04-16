
open Config
open Canvas
open Sketch
open Renderer
open Key

(** Used internally by dynamic runner *)
module Dynamic : sig
  type dynamic_handler = unit -> unit
  val set_dynamic_handler : dynamic_handler -> unit
  val apply_handler : unit -> unit
  val is_dynamic : unit -> bool
  val set_initialized : unit -> unit
  val is_initialized : unit -> bool
  val set_dynamic_hotswap : (Obj.t -> config -> Obj.t -> unit) -> unit
  val apply_dynamic_hotswap : 'a -> config -> 'b -> unit
  val has_dynamic_hotswap : unit -> bool
end = struct
  type dynamic_handler = unit -> unit
    
  let handler : dynamic_handler option ref = ref None
      
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
end

module Runner (S : Sketch) : sig
  val run : unit -> unit
  val load_dynamic_module : Obj.t -> config -> Obj.t -> unit
end = struct
  let target_frame_rate = 60.

  let default_background_color = Color.gray 127

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

  let update_dynamic () =
    if Dynamic.is_dynamic ()
    then Dynamic.apply_handler ()
    else ()

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
      in if key = Key.esc then
        begin
          if Dynamic.is_dynamic ()
          then (update_dynamic (); config, state)
          else raise Exit
        end
      else let config' = {config with key_pressed = true; key = key; key_unicode = unicode}
        in config', S.key_pressed config' state
    | KeyReleased unicode ->
      let key = char_of_unicode unicode
      in let config' = {config with key_pressed = false; key = key; key_unicode = unicode}
      in config', S.key_released config' state |> S.key_typed config'
    | WindowResized {width; height} ->
      let config' = {config with width = width; height = height}
      in config', S.window_resized config' state
    | WindowClosed -> config, S.window_closed config state

  let default_background buffer =
    let module C = Canvas (S.R) in
    C.background default_background_color

  let rec loop buffer config state =
    let start = Unix.gettimeofday ()
    in let config' = loop_config config
    in let config'', state' =
         List.fold_left handle_event (config', state) (S.R.event_queue buffer)
    in let state'' = S.loop config' state'
    in let painter = S.R.comp [
        default_background buffer;
        S.draw config'' state'';
      ]
    in let base_paint = Paint.create
    in begin
      S.R.begin_draw buffer;
      S.R.clear buffer;
      S.R.paint buffer base_paint painter;
      S.R.end_draw buffer;
      Unix.sleepf (max 0.005 (1. /. target_frame_rate -. (Unix.gettimeofday () -. start)));
      if Dynamic.has_dynamic_hotswap ()
      then Dynamic.apply_dynamic_hotswap buffer config'' state''
      else loop buffer config'' state''
    end

  let wrap_handle_exns func =
    try func () with
    | Exit -> ()

  let run () =
    wrap_handle_exns
      begin fun () ->
        let buffer = S.R.create_buffer target_frame_rate
        in let config = create_config buffer
        in let state = S.setup config
        in loop buffer config state
      end

  let load_dynamic_module buffer_mag config state_mag =
    wrap_handle_exns
      begin fun () ->
        (* do not try this at home kids *)
        loop (Obj.obj buffer_mag) config (Obj.obj state_mag)
      end
end

let run_sketch (sketch : (module Sketch)) : unit =
  let module Run = Runner (val sketch : Sketch) in
  begin
    if Dynamic.is_initialized ()
    then Dynamic.set_dynamic_hotswap Run.load_dynamic_module
    else (Dynamic.set_initialized (); Run.run ())
  end
