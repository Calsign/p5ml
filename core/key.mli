
(** [Key] contains preset key values for [config]'s [key] and [key_unicode]
    fields. *)

module Key : sig
  val backspace : char
  val tab : char
  val enter : char
  val return : char
  val esc : char
  val delete : char
end

module KeyUnicode : sig
  val backspace : Uchar.t
  val tab : Uchar.t
  val enter : Uchar.t
  val return : Uchar.t
  val esc : Uchar.t
  val delete : Uchar.t

  val left : Uchar.t
  val up : Uchar.t
  val right : Uchar.t
  val down : Uchar.t

  val shift_l : Uchar.t
  val shift_r : Uchar.t
  val control_l : Uchar.t
  val control_r : Uchar.t
  val caps_lock : Uchar.t
  val meta_l : Uchar.t
  val meta_r : Uchar.t
  val alt_l : Uchar.t
  val alt_r : Uchar.t
end
