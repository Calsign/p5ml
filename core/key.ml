
module Key = struct
  let backspace = '\008'
  let tab = '\009'
  let enter = '\010'
  let return = '\013'
  let esc = '\027'
  let delete = '\127'
end

module KeyUnicode = struct
  (* https://gitlab.gnome.org/GNOME/gtk/raw/master/gdk/gdkkeysyms.h *)

  open Uchar

  let backspace = of_int 0xff08
  let tab = of_int 0xff09
  let enter = of_int 0xff0d
  let return = enter
  let esc = of_int 0xff1b
  let delete = of_int 0xffff

  let left = of_int 0xff51
  let up = of_int 0xff52
  let right = of_int 0xff53
  let down = of_int 0xff54

  let shift_l = of_int 0xffe1
  let shift_r = of_int 0xffe2
  let control_l = of_int 0xffe3
  let control_r = of_int 0xffe4
  let caps_lock = of_int 0xffe5
  let meta_l = of_int 0xffe7
  let meta_r = of_int 0xffe8
  let alt_l = of_int 0xffe9
  let alt_r = of_int 0xffea
end
