
let string_of_char c = String.make 1 c

let char_of_string s =
  match String.length s with
  | 1 -> String.get s 0
  | _ -> failwith "char_of_string"
