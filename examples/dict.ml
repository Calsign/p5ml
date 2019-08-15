
(* Press a key to see a different circle appear. *)

open P5.Gtkc

let keys = [
  ("a", ((100., 100.), gray 255));
  ("b", ((200., 100.), rgb 255 0 0));
  ("c", ((500., 500.), rgb 0 255 0));
]

module DictSketch : Sketch = struct
  include Base

  type state = (vector * color) Dict.t

  let display = `Size (1200, 600)

  let setup conf =
    List.fold_left (fun d (k, v) -> Dict.add k v d) Dict.empty keys

  let draw conf st =
    let selected =
      match Dict.find_opt (string_of_char conf.key) st with
      | Some (vec, col) -> circle vec 100. |> fill col |> no_stroke
      | None -> empty
    in group [background (gray 0); selected]
end

let () = run_sketch (module DictSketch)
