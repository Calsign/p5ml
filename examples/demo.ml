
open P5.Graph

module TestSketch = struct
  include Base

  type state = {x : int}

  let setup conf = {x = 0}

  let loop conf st =
    {x = ((st.x + 2) mod (conf.width))}

  let draw conf st = comp [
      background (gray 0);
      ellipse 100 100 50 50 |> stroke (gray 255);
      comp [
        rect st.x 100 100 100;
        rect (st.x - conf.width) 100 100 100;
      ] |> stroke (rgb 255 0 0) |> no_fill |> stroke_weight 3.;
      ellipse (conf.mouse_x - 10) (conf.mouse_y - 10) 20 20 |> fill (rgb 0 255 0);
    ]

  let mouse_pressed conf st = {x = st.x - 50}

  let key_pressed conf st =
    (match conf.key with
    | k when k = Key.return -> print_endline "return"; raise Exit
    | _ -> ()); st
end

let () = run_sketch (module TestSketch)
