
type color = {
  red : int;
  green : int;
  blue : int;
  alpha : int;
}

let constrain_color c =
  min 255 (max c 0)

let rgba r g b a = {
  red = constrain_color r;
  green = constrain_color g;
  blue = constrain_color b;
  alpha = constrain_color a;
}

let rgb r g b = rgba r g b 255

let gray v = rgba v v v 255

let hsv h s v = failwith "HSV color unimplemented"

let hsva h s v a = {(hsv h s v) with alpha = a}

let hex hex = failwith "Hexadecimal color unimplemented"
