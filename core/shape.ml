
open Math

module Shape = struct
  type vector = Vector.t

  type vertex =
    | MoveTo of vector
    | LineTo of vector
    | Bezier of vector * vector * vector
  and t =
    | Group of t list
    | Shape of {vertices : vertex list; contour : t option}

  let build_shape hd tl =
    List.fold_left
      begin fun acc -> function
        | `Vertex vec -> LineTo vec :: acc
        | `Bezier (c1, c2, a2) -> Bezier (c1, c2, a2) :: acc
      end [MoveTo hd] tl

  let build_poly sides hd tl =
    List.fold_left
      begin fun (acc, flag) next -> match next, flag with
        | `Vertex vec, n | `Bezier (_, _, vec), n when n = sides - 1 ->
          MoveTo vec :: acc, 0
        | `Vertex vec, n -> LineTo vec :: acc, n + 1
        | `Bezier (c1, c2, a2), n -> Bezier (c1, c2, a2) :: acc, n + 1
      end ([MoveTo hd], 1) tl |> fst

  let build_triangle_strip hd tl =
    List.fold_left
      begin fun (acc, prev1, prev2) next ->
        let seg, pt = match next with
          | `Vertex vec -> LineTo vec, vec
          | `Bezier (c1, c2, a2) -> Bezier (c1, c2, a2), a2
        in begin match prev2 with
        | Some p -> LineTo pt :: MoveTo p :: seg :: acc
        | None -> seg :: acc
        end, Some pt, prev1
      end ([MoveTo hd], None, None) tl |> fun (lst, _, _) -> lst

  let build_triangle_fan hd tl =
    List.fold_left
      begin fun acc -> function
        | `Vertex vec ->
          MoveTo vec :: LineTo hd :: LineTo vec :: acc
        | `Bezier (c1, c2, a2) ->
          MoveTo a2 :: LineTo hd :: Bezier (c1, c2, a2) :: acc
      end [MoveTo hd] tl

  let build_quad_strip hd tl =
    List.fold_left
      begin fun (acc, prev1, prev2, prev3) next ->
        let seg, pt = match next with
          | `Vertex vec -> LineTo vec, vec
          | `Bezier (c1, c2, a2) -> Bezier (c1, c2, a2), a2
        in begin match prev3 with
          | Some p -> LineTo pt :: MoveTo p :: seg :: acc
          | None -> seg :: acc
        end, Some pt, prev1, prev2
      end ([MoveTo hd], None, None, None) tl |> fun (lst, _, _, _) -> lst

  let create ?(kind = `Shape) ?(cap = `Open) ?contour vertices =
    let vertices' = match vertices with
      | hd :: tl ->
        let head, tail =
          begin
            match hd with
            | `Vertex vec -> vec, tl
            | `Bezier (c1, c2, a2) -> c1, (`Bezier (c2, c2, a2)) :: tl
          end
        in begin
          match kind with
          | `Shape -> build_shape
          | `Points -> build_poly 1
          | `Lines -> build_poly 2
          | `Triangles -> build_poly 3
          | `Triangle_strip -> build_triangle_strip
          | `Triangle_fan -> build_triangle_fan
          | `Quads -> build_poly 4
          | `Quad_strip -> build_quad_strip
        end head tail |> begin fun lst -> match cap with
          | `Open -> lst
          | `Closed -> LineTo head :: lst
        end
      | [] -> []
    in Shape {vertices = vertices'; contour = contour}
    
  let group shapes = Group shapes
end
