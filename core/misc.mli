
(** [Misc] contains miscellaneous utility functions. *)

(** [string_of_char c] is the string containing the character [c]. *)
val string_of_char : char -> string

(** [char_of_string s] is the sole character in [s] if [s] has length one;
    raises [Failure "char_of_string"] otherwise. *)
val char_of_string : string -> char
