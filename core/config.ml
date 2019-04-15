
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
  key_unicode : Uchar.t;
  key_pressed : bool;

  frame_count : int;
  frame_rate : float;
}

(* this is a weird place to put this, but it's here for now *)
exception Exit
