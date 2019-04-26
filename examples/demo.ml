
open P5.Gtkc

module TestSketch = struct
  include Base

  type state = {x : float; y : int; z : int}

  let setup conf = {x = 0.; y = 0; z = 0}

  let loop conf st =
    {st with x = (mod_float (st.x +. 2.) (float_of_int conf.width))}

  let curve = Bezier.create (700., 100.) (800., 50.) (800., 250.) (700., 200.)

  let draw conf st = group [
      background (gray 0);
      ellipse (150., 100.) (150., 100.)
      |> stroke (gray 255) |> no_fill |> stroke_weight 10.;
      ellipse (100., 200.) (50., 50.) ~align:`Center |> stroke (gray 255) |> no_fill |> stroke_weight 10.;
      group [
        rect (st.x, 100.) (100., 100.);
        rect (st.x -. ~.(conf.width), 100.) (100., 100.);
      ] |> stroke (rgb 255 0 0) |> fill (graya 255 100) |> stroke_weight 3.;
      point (~.(conf.mouse_x), ~.(conf.mouse_y)) |> stroke (rgb 0 255 0) |> stroke_weight 20.;
      line (200., 200.) (400., 200.) |> stroke (gray 255) |> stroke_weight 30. |> stroke_cap `Square;
      rect (100., 400.) (100., 100.) |> stroke (rgb 0 255 255) |> no_fill |> stroke_weight 30. |> stroke_join `Bevel;
      arc (500., 100.) (100., 100.) 0. ~stroke_mode:`Closed ~fill_mode:`Pie
        (Math.half_pi *. 3.) |> stroke_weight 10. |> stroke (rgb 255 0 0);
      bezier curve |> stroke (gray 255) |> no_fill;
      List.init 21 (fun n ->
          let open Vector
          in let t = (float_of_int n) /. 20.
          in let pnt = Bezier.interpolate curve t
          in let tgt = pnt ++ ((Bezier.tangent curve t) ** 0.3)
          in group [
            point pnt |> stroke_weight 3.;
            line pnt tgt;
          ]) |> group |> translate (100., 0.) |> stroke (rgb 255 0 0);
      line (500.,500.) (600.,500.) |> stroke (gray 255) |> stroke_weight 10.;
    ]

  let mouse_pressed conf st =
    match conf.mouse_button with
    | `Left -> {st with x = st.x -. 50.}
    | `Center -> st
    | `Right -> {st with x = st.x +. 50.}

  let key_pressed conf st =
    (match conf.key with
    | k when k = Key.enter -> raise Exit
    | _ -> ()); st

  let mouse_scrolled conf st =
    {st with x = st.x +. (~. (conf.mouse_scroll) *. 10.)}
end

let () = run_sketch (module TestSketch)
