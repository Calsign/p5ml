
module KeysSet = Set.Make (Uchar)

type keys = KeysSet.t

type config = {
  width : int;
  height : int;
  display_width : int;
  display_height : int;

  mouse_x : int;
  mouse_y : int;
  pmouse_x : int;
  pmouse_y : int;
  mouse_scroll : int;
  mouse_pressed : bool;
  mouse_button : [`Left | `Right | `Center];

  key : char;
  keys : keys;
  key_unicode : Uchar.t;
  key_pressed : bool;

  frame_count : int;
  frame_rate : float;
}

let check_key_uchar conf uchar =
  KeysSet.mem uchar conf.keys

let check_key conf char =
  check_key_uchar conf (Uchar.of_char char)

let get_keys_uchar conf =
  KeysSet.fold (List.cons) conf.keys []

let get_keys conf =
  get_keys_uchar conf |> List.filter Uchar.is_char |> List.map Uchar.to_char

(* this is a weird place to put this, but it's here for now *)
exception Exit
