
open P5.Gtkc

module TestSketch = struct
  include Base

  type state = {x : int; y : int}

  let setup conf = {x = 0; y = 0}

  let loop conf st =
    {st with x = ((st.x + 2) mod (conf.width))}

  let curve = Bezier.create 700 100 800 50 800 250 700 200

  let draw conf st = comp [
      background (gray 0);
      ellipse 100 100 100 50 |> stroke (gray 255) |> no_fill |> stroke_weight 10.;
      ellipse 100 200 50 50 ~align:`Center |> stroke (gray 255) |> no_fill |> stroke_weight 10.;
      comp [
        rect st.x 100 100 100;
        rect (st.x - conf.width) 100 100 100;
      ] |> stroke (rgb 255 0 0) |> fill (graya 255 100) |> stroke_weight 3.;
      point conf.mouse_x conf.mouse_y |> stroke (rgb 0 255 0) |> stroke_weight 20.;
      line 200 200 400 200 |> stroke (gray 255) |> stroke_weight 30. |> stroke_cap `Square;
      rect 100 400 100 100 |> stroke (rgb 0 255 255) |> no_fill |> stroke_weight 30. |> stroke_join `Bevel;
      arc 500 100 100 100 0. ~stroke_mode:`Closed ~fill_mode:`Pie
        (Math.half_pi *. 3.) |> stroke_weight 10. |> stroke (rgb 255 0 0);
      bezier curve |> stroke (gray 255);
      List.init 21 (fun n ->
          let t = (float_of_int n) /. 20.
          in let x, y = Bezier.interpolate curve t
          in let dx, dy = Bezier.tangent curve t
          in let px = x + (int_of_float (dx *. 0.3))
          in let py = y + (int_of_float (dy *. 0.3))
          in comp [
            point (x + 100) y |> stroke_weight 3.;
            line (x + 100) y (px + 100) py;
          ]) |> comp |> stroke (rgb 255 0 0);
      shape Shape.(create ~cap:`Closed [
          vert 500 500;
          vert 600 500;
          vert 550 600;
          vert_bezier 540 650 510 650 480 600;
        ]) |> stroke (rgb 255 0 0) |> fill (rgb 100 0 150) |> stroke_weight 5.;
      shape Shape.(create ~kind:`Triangle_strip [
          vert 700 700;
          vert 800 800;
          vert 800 700;
          vert 1000 800;
          vert 1000 900;
          vert 900 1000;
        ]) |> stroke_weight 10. |> stroke (gray 127);
    ]

  let mouse_pressed conf st =
    match conf.mouse_button with
    | `Left -> {st with x = st.x - 50}
    | `Center -> st
    | `Right -> {st with x = st.x + 50}

  let key_pressed conf st =
    (match conf.key with
    | k when k = Key.enter -> raise Exit
    | _ -> ()); st

  let mouse_scrolled conf st =
    {st with x = st.x + conf.mouse_scroll * 10}
end

let () = run_sketch (module TestSketch)
