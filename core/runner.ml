
open Config
open Canvas
open Sketch
open Renderer
open Key

exception Exit

module Runner (S : Sketch) : sig
  val run : unit -> unit
end = struct
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
      mouse_button = None;

      key = Char.chr 0;
      key_unicode = Uchar.min;
      key_pressed = false;
    }

  let frame_rate () = 60.

  let loop_config config =
    {
      config with
      pmouse_x = config.mouse_x;
      pmouse_y = config.mouse_y;
      mouse_scroll = 0;
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

  let handle_event (config, state) = function
    | MousePressed ({x; y}, button) ->
      let config' = update_config_mouse config x y button true
      in config', S.mouse_pressed config' state
    | MouseMoved {x; y} ->
      let config' = update_config_mouse config x y config.mouse_button true
      in config', if config.mouse_pressed
         then S.mouse_dragged config' state else state
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
      else let config' = {config with key_pressed = true; key = key; key_unicode = unicode}
        in config', S.key_pressed config' state
    | KeyReleased unicode ->
      let key = char_of_unicode unicode
      in let config' = {config with key_pressed = false; key = key; key_unicode = unicode}
      in config', S.key_released config' state |> S.key_typed config'
    | WindowResized {width; height} ->
      let config' = {config with width = width; height = height}
      in config', S.window_resized config' state

  let rec loop buffer config state =
    let start = Unix.gettimeofday ()
    in let config' = loop_config config
    in let config'', state' =
         List.fold_left handle_event (config', state) (S.R.event_queue buffer)
    in let state'' = S.loop config' state'
    in let painter = S.draw config'' state''
    in let base_paint = Paint.create
    in begin
      S.R.begin_draw buffer;
      S.R.clear buffer;
      S.R.paint buffer base_paint painter;
      S.R.end_draw buffer;
      Unix.sleepf (max 0.005 (1. /. (frame_rate ()) -. (Unix.gettimeofday () -. start)));
      loop buffer config'' state''
    end

  let run () =
    try begin
      let buffer = S.R.create_buffer (frame_rate ())
      in let config = create_config buffer
      in let state = S.setup config
      in loop buffer config state
    end with
    | End_of_file -> ()
    | Exit -> ()
end

let run_sketch sketch =
  let module Run = Runner (val sketch : Sketch)
  in Run.run ()
