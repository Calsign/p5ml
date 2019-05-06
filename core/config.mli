
type keys = Set.Make (Uchar).t

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

(** {2 Keys} *)

(** [check_key config char] is [true] if the key [char] is pressed in [config]
    and [false] otherwise. *)
val check_key : config -> char -> bool

(** [check_key_uchar config char] is [true] if the key [char] is pressed in
    [config] and [false] otherwise. Supports unicode characters. *)
val check_key_uchar : config -> Uchar.t -> bool

(** [get_keys config] is the list of all keys that are currently pressed in
    [config], or [[]] if there are no keys pressed. Only includes keys that
    are valid ASCII char values. *)
val get_keys : config -> char list

(** [key_keys_uchar config] is the list of all keys that are currently pressed
    in [config], or [[]] if there are no keys pressed. Includes all keys,
    represented as Unicode characters. *)
val get_keys_uchar : config -> Uchar.t list

(** {2 Exit} *)

exception Exit
