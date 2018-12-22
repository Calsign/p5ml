
type mouse_button = Left | Right | Center | None

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
  mouse_button : mouse_button;

  key : char;
  key_code : int;
  key_pressed : bool;
}
