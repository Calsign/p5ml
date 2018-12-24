
open Color

type paint = {
  fill : color option;
  stroke : color option;
  stroke_weight : float;
  stroke_cap : [`Round | `Square | `Project];
  stroke_join : [`Miter | `Bevel | `Round];
}

let fill color paint = {paint with fill = Some color}

let stroke color paint = {paint with stroke = Some color}

let no_fill paint = {paint with fill = None}

let no_stroke paint = {paint with stroke = None}

let stroke_weight weight paint = {paint with stroke_weight = max weight 0.}

let stroke_cap cap paint = {paint with stroke_cap = cap}

let stroke_join join paint = {paint with stroke_join = join}

let create = {
  fill = Some (gray 255);
  stroke = Some (gray 0);
  stroke_weight = 1.;
  stroke_cap = `Round;
  stroke_join = `Miter;
}
