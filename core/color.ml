
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

let graya v a = rgba v v v a

let gray v = graya v 255

let hsv h s v =
  if h < 0 || h > 360
  then failwith ("hsv h out of range [0, 360]: " ^ string_of_int h) else
  if s < 0 || s > 100
  then failwith ("hsv s out of range [0, 100]: " ^ string_of_int s) else
  if v < 0 || v > 100
  then failwith ("hsv v out of range [0, 100]: " ^ string_of_int v) else
  let open Math in
  let sat = ~.s /. 100. in
  let value = ~.v /. 100. in
  let chroma = value *. sat in
  let h' = ~.h /. 60. in
  let x = chroma *. (1. -. Math.absf ((mod_float h' 2.) -. 1.)) in
  let r', g', b' =
    if h' >= 0. && h' < 1. then chroma, x, 0. else
    if h' >= 1. && h' < 2. then x, chroma, 0. else
    if h' >= 2. && h' < 3. then 0., chroma, x else
    if h' >= 3. && h' < 4. then 0., x, chroma else
    if h' >= 4. && h' < 5. then x, 0., chroma else
    if h' >= 5. && h' <= 6. then chroma, 0., x else 0., 0., 0. in
  let m = value -. chroma in
  rgb
    ((r' +. m) *. 255. |> Math.round)
    ((g' +. m) *. 255. |> Math.round)
    ((b' +. m) *. 255. |> Math.round)

let hsva h s v a = {(hsv h s v) with alpha = a}

let hex hex =
  (* remove leading pound sign *)
  let hex' =
    if String.length hex > 0 && String.get hex 0 = '#'
    then String.sub hex 1 (String.length hex - 1)
    else hex in
  (* add alpha characters if not present *)
  let hex'' =
    if String.length hex' = 6 then "FF" ^ hex'
    else if String.length hex' = 8 then hex'
    else failwith ("Invalid hex color: " ^ hex) in
  (* convert a hex digit [0-9A-Za-z] to an integer *)
  let int_of_hex_digit c =
    if Char.code c >= Char.code 'a' && Char.code c <= Char.code 'f'
    then Char.code c - Char.code 'a' + 10
    else if Char.code c >= Char.code 'A' && Char.code c <= Char.code 'F'
    then Char.code c - Char.code 'A' + 10
    else if Char.code c >= Char.code '0' && Char.code c <= Char.code '9'
    then Char.code c - Char.code '0'
    else failwith ("Invalid hex char '" ^ (String.make 1 c) ^ " in: " ^ hex) in
  (* convert two hex digits into an integer *)
  let int_of_hex_pair c1 c2 =
    16 * (int_of_hex_digit c1) + (int_of_hex_digit c2) in
  (* convert two sequential hex digits from [hex] into an integer *)
  let int_of_str_pair_pos i =
    int_of_hex_pair (String.get hex'' i) (String.get hex'' (i + 1)) in
  let a = int_of_str_pair_pos 0 in
  let r = int_of_str_pair_pos 2 in
  let g = int_of_str_pair_pos 4 in
  let b = int_of_str_pair_pos 6 in
  rgba r g b a
