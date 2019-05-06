
open P5.Gtkc
open Vector

module Pong = struct
  include Base

  type state =
    {
      pos : Vector.t; (* ball position *)
      vel : Vector.t; (* ball velocity *)
      paddle_a : float; (* paddle A y position *)
      paddle_b : float; (* paddle B y position *)
    }

  let size = 1200, 800

  let setup conf =
    {
      pos = (~.(conf.width / 2), ~.(conf.height / 2));
      vel = (-5., 0.);
      paddle_a = ~.(conf.height / 2);
      paddle_b = ~.(conf.height / 2);
    }

  (* how far in the paddles are from the sides horizontally *)
  let paddle_bound = 100.
  (* how tall the paddles are (y) *)
  let paddle_height = 100.
  (* how wide the paddles are (x) *)
  let paddle_width = 30.
  (* how far in the paddles are from the sides vertically *)
  let side_bound = paddle_height /. 2.
  (* the radius of the ball *)
  let ball_radius = 15.
  (* ball minimum x speed *)
  let min_x_speed = 10.
  (* ball maximum x speed *)
  let max_x_speed = 12.
  (* ball maximum y speed (minimum is 0) *)
  let max_y_speed = 10.
  (* how far the paddle moves up or down with each key press *)
  let paddle_motion = 10.

  (** [constrain_paddle conf paddle_y] is [paddle_y] constrained to be within
      the permitted bounds for [conf]. *)
  let constrain_paddle conf y =
    Math.constrainf y (side_bound +. paddle_height /. 2.)
      (~.(conf.height) -. side_bound -. paddle_height /. 2.)

  (** [handle_keys conf st key] is [st'], where [st'] is the state resulting
      from [key] being pressed in [st] with [conf] for a frame. *)
  let handle_keys conf st key =
    (* player A uses 'a' and 'z' to move up and down
       player B uses ''' and '/' to move up and down *)
    match key with
    | 'a' ->
      {st with paddle_a = constrain_paddle conf (st.paddle_a -. paddle_motion)}
    | 'z' ->
      {st with paddle_a = constrain_paddle conf (st.paddle_a +. paddle_motion)}
    | '\'' ->
      {st with paddle_b = constrain_paddle conf (st.paddle_b -. paddle_motion)}
    | '/' ->
      {st with paddle_b = constrain_paddle conf (st.paddle_b +. paddle_motion)}
    | _ -> st

  let loop conf st =
    (* unpack *)
    let px, py = st.pos in
    let vx, vy = st.vel in
    (* check to see if a player has missed the ball *)
    let out_x =
      (px < -. ball_radius && vx < 0.) ||
      (px > ~.(conf.width) +. ball_radius && vx >= 0.) in
    (* check to see if the ball bounces off the bottom or top *)
    let bounce_y =
      (py < side_bound +. ball_radius && vy < 0.) ||
      (py > ~.(conf.height) -. side_bound -. ball_radius && vy >= 0.) in
    (* check to see if the ball bounces off of the paddles *)
    let bounce_x =
      (vx < 0. && Math.absf
         (px -. (paddle_bound +. paddle_width /. 2.
                 +. ball_radius)) <= max_x_speed
       && Math.absf (py -. st.paddle_a) < paddle_height /. 2. +. ball_radius) ||
      (vx >= 0. && Math.absf
         (px -. (~.(conf.width) -. paddle_bound
                 -. paddle_width /. 2. -. ball_radius)) <= max_x_speed
       && Math.absf (py -. st.paddle_b) < paddle_height /. 2. +. ball_radius) in
    let vx', vy' = match bounce_x with
      (* on bounce *)
      | true ->
        begin
          (* x velocity is randomized *)
          (vx /. (Math.absf vx)) *.
          (Math.random_float ~lower_bound:min_x_speed max_x_speed),
          (* y velocity is also randomized *)
          Math.constrainf ((Math.random_float ~lower_bound:(-5.) 5.) +. vy)
            (-.max_y_speed) max_y_speed
        end
      (* no bounce, stay the same *)
      | false -> vx, vy in
    let pos', vel' = match out_x with
      (* on out of bounds *)
      | true ->
        begin
          (* return to center *)
          (~.(conf.width) /. 2., ~.(conf.height) /. 2.),
          (* go slow for first hit *)
          ((~.(Math.random_int 2) -. 0.5) *. 6., 0.)
        end
      | false ->
        (* no out of bounds, step like normal *)
        begin
          st.pos ++ st.vel,
          (* if we bounce, flip direction *)
          (vx' *. (if bounce_x then -1. else 1.),
           vy' *. (if bounce_y then -1. else 1.))
        end in
    {
      st with
      pos = pos';
      vel = vel';
    } |> fun st -> if conf.key_pressed
    (* handle all keys currently pressed *)
    then List.fold_left (handle_keys conf) st (get_keys conf) else st

  let draw conf st = group
      [
        background (gray 0);
        (* lines at top and bound denoting field of play *)
        group
          [
            line (0., side_bound) (~.(conf.width), side_bound);
            line (0., ~.(conf.height) -. side_bound)
              (~.(conf.width), ~.(conf.height) -. side_bound);
          ] |> stroke (gray (128));
        (* ball *)
        rect st.pos ~align:`Center (ball_radius *. 2., ball_radius *. 2.);
        (* paddles *)
        rect (paddle_bound, st.paddle_a)
          ~align:`Center (paddle_width, paddle_height);
        rect (~.(conf.width) -. paddle_bound, st.paddle_b)
          ~align:`Center (paddle_width, paddle_height);
      ] |> fill (gray 255) |> no_stroke
end

let () = run_sketch (module Pong)
