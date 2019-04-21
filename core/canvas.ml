
open Config
open Color
open Paint
open Math
open Bezier
open Shape
open Renderer

module Canvas (R : Renderer) : sig
  val background : config -> color -> Shape.t
end = struct
  let background config color =
    rect (0., 0.) (float_of_int config.width,
                   float_of_int config.height)
    |> no_stroke |> fill color
end
