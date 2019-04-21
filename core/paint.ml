
open Color

type paint = {
  fill : color option;
  stroke : color option;
  stroke_weight : float;
  stroke_cap : [`Round | `Square | `Project];
  stroke_join : [`Miter | `Bevel | `Round];
}

let create = {
  fill = Some (gray 255);
  stroke = Some (gray 0);
  stroke_weight = 1.;
  stroke_cap = `Round;
  stroke_join = `Miter;
}

type paint_update =
  | Fill of color option
  | Stroke of color option
  | Stroke_weight of float
  | Stroke_cap of [`Round | `Square | `Project]
  | Stroke_join of [`Miter | `Bevel | `Round]

let apply_paint_update paint_update paint =
  match paint_update with
  | Fill color -> {paint with fill=color}
  | Stroke color -> {paint with stroke=color}
  | Stroke_weight weight -> {paint with stroke_weight=weight}
  | Stroke_cap cap -> {paint with stroke_cap=cap}
  | Stroke_join join -> {paint with stroke_join=join}
