
(* Performance stress-test.
   Click or press any key to double the number of rectangles. *)

open P5.Gtkc

module Stress = struct
  include Base

  type state = (vector * vector) list * int

  let dim = 30., 30.
  let start_num = 100

  let display = `Size (1200, 1200)

  let reset_list conf num =
    List.init num (fun _ ->
        (Math.random_float ~.(conf.width),
         Math.random_float ~.(conf.height)),
        (Math.random_float ~lower_bound:(-5.) 5.,
         Math.random_float ~lower_bound:(-5.) 5.))

  let setup conf =
    reset_list conf start_num, start_num

  let vec_mod (x, y) (w, h) =
    (if x < 0. then w +. (mod_float x w) else (mod_float x w)),
    (if y < 0. then h +. (mod_float y h) else (mod_float y h))

  let loop conf (lst, num) =
    conf.frame_rate |> string_of_float |> print_endline;
    List.map Vector.(fun (pos, vel) ->
        vec_mod (pos ++ vel)
          ((~.(conf.width), ~.(conf.height)) ++ dim), vel) lst, num

  let draw conf (lst, _) =
    group (List.map (fun (pos, _) -> rect Vector.(pos -- dim) dim) lst)

  let increase_num conf (lst, num) =
    let num' = num * 2 in
    reset_list conf num', num'

  let mouse_pressed = increase_num
  let key_pressed = increase_num
end

let () = run_sketch (module Stress)
