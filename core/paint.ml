
open Color

type paint = {
  fill : color option;
  stroke : color option;
  stroke_weight : float;
}

let fill color paint = {paint with fill = Some color}

let stroke color paint = {paint with stroke = Some color}

let no_fill paint = {paint with fill = None}

let no_stroke paint = {paint with stroke = None}

let stroke_weight weight paint = {paint with stroke_weight = max weight 0.}

let extr_fill paint = paint.fill

let extr_stroke paint = paint.stroke

let extr_stroke_weight paint = paint.stroke_weight

let create = {
  fill = Some (gray 255);
  stroke = Some (gray 0);
  stroke_weight = 1.;
}
