
module A = BatArray

type winding =
| WINDING_CLOCKWISE
| WINDING_COUNTERCLOCKWISE
| WINDING_NONE

module Point = struct

  type t = float * float

  let x (p: t) = let x, _ = p in x
  let y (p: t) = let _, y = p in y

  let add pt1 pt2 =
    let x1, y1 = pt1 in
    let x2, y2 = pt2 in
    (x2 +. x1, y2 +. y1)

  let sub pt1 pt2 =
    let x1, y1 = pt1 in
    let x2, y2 = pt2 in
    (x2 -. x1, y2 -. y1)

  let distance (pt1, pt2) =
    let dx, dy = sub pt1 pt2 in
    sqrt (dx *. dx +. dy *. dy)

  let as_string (x, y) =
    "(" ^ (string_of_float x) ^ ", " ^ (string_of_float y) ^ ")"

end

module Polygon = struct

  type t = Point.t array

  (*
  let signed_double_area (poly: t) =
    let rec aux vertex sum =
      if vertex = Array.length poly then
        sum
      else
        let next_vertex = (vertex + 1) mod n in
        let vx, vy = poly.(vertex) in
        let nx, ny = poly.(next_vertex) in
        aux (vertex + 1) (sum + vx *. ny - nx *. vy)
    in
    aux 0 0.0
  *)

  let signed_double_area (poly: t) =
    let n = A.length poly in
    let subtriangle_area sum i (vx, vy) =
      let next_vertex = (i + 1) mod n in
      let nx, ny = poly.(next_vertex) in
      sum +. vx *. ny -. nx *. vy
    in
    A.fold_lefti subtriangle_area 0.0 poly

  let area poly =
    abs_float ((signed_double_area poly) /. 2.0)

  let winding poly =
    match signed_double_area poly with
    | sda when sda < 0.0 -> WINDING_CLOCKWISE
    | sda when sda > 0.0 -> WINDING_COUNTERCLOCKWISE
    | _ -> WINDING_NONE

end

module LineSegment = struct

  type t = Point.t * Point.t

  let cmp_len_max ((ls1: t), (ls2: t)) =
    let len1 = Point.distance ls1 in
    let len2 = Point.distance ls2 in
    if len1 < len2 then 1
    else if len1 > len2 then -1
    else 0

  let cmp_len (edge1, edge2) =
    -(cmp_len_max (edge1, edge2))

end

module Circle = struct

  type t = { center: Point.t; radius: float; }

  let as_string c =
    "Circle (center: " ^ (Point.as_string c.center)
      ^ "; radius: " ^ (string_of_float c.radius) ^ ")"

end

