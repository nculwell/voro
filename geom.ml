
module A = BatArray
module L = BatList

type winding =
| WindingClockwise
| WindingCounterclockwise
| WindingNone

let fequal a b = abs_float (a -. b) < 0.000000015

module Point = struct

  type t = float * float

  let x (p: t) = let x, _ = p in x
  let y (p: t) = let _, y = p in y

  let equal pt1 pt2 =
    let x1, y1 = pt1 in
    let x2, y2 = pt2 in
    fequal x1 x2 && fequal y1 y2

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

module Size = struct
  type t = float * float

  let area (s: t): float = let w, h = s in w *. h
end

module Line3 = struct

  (* (a, b, c) denotes the line given by ax + by = c *)
  type t = float * float * float

  let contains_point ((a, b, c): t) ((x, y): Point.t) =
    if fequal c 0.0 then
      if fequal b 0.0 || fequal y 0.0 then
        fequal x 0.0
      else
        fequal ((-. a *. x) /. (b *. y)) 1.0
    else
      fequal ((a *. x +. b *. y) /. c) 1.0

end

module Polygon = struct

  type t = Point.t array

  let empty: t = [| |]

  let from_list (points: Point.t list): t =
    A.of_list points

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
    | sda when sda < 0.0 -> WindingClockwise
    | sda when sda > 0.0 -> WindingCounterclockwise
    | _ -> WindingNone

  let as_string (poly: t): string =
    "Polygon ("
      ^ (poly |> A.map Point.as_string |> A.to_list |> String.concat ", ")
      ^ ")"

end

module HPlane3 = struct

  (* (a, b, c) denotes the halfplane given by ax + by <= c *)
  type t = float * float * float

  let contains_point ((a, b, c): t) ((x, y): Point.t): bool =
    Line3.contains_point (a, b, c) (x, y)
      || a *. x +. b *. y <= c

  let contains_polygon (hp: t) (poly: Polygon.t): bool =
    try
      let pt = A.find (fun p -> not (Line3.contains_point hp p)) poly in
      contains_point hp pt
    with
    | Not_found -> true (* degenerate polygon *)

  let flip ((a, b, c): t): t = (-. a, -. b, -. c)

end

module LineSegment = struct

  type t = Point.t * Point.t

  let cmp_len_max ((ls1: t), (ls2: t)): float =
    let len1 = Point.distance ls1 in
    let len2 = Point.distance ls2 in
    if len1 < len2 then 1.0
    else if len1 > len2 then -1.0
    else 0.0

  let cmp_len (edge1, edge2) =
    -. (cmp_len_max (edge1, edge2))


  (* If the line intersects the segment at point m, return Some m.  Otherwise
   * return None. By convention, if the segment is coincident with the line,
   * return Some p. Caller warrants that the two points are not the same
   * point.
   *
   * mx = x1 + u * (x2 - x1)
   * my = y1 + u * (y2 - y1)
   * a * mx + b * my = c
   *
   * u = (c - b * y1 - a * x1) / (a * (x2 - x1) + b * (y2 - y1))
   *)
  let intersects_line
      ((a, b, c): Line3.t)
      (((x1, y1), (x2, y2)): t)
      : Point.t option =
    if x1 = x2 && y1 = y2 then
      failwith "Points in line segment are the same."
    else
      let dx, dy = x2 -. x1, y2 -. y1 in
      let parallel =
        if fequal a 0.0 then
          fequal dy 0.0
        else if fequal dx 0.0 then
          fequal b 0.0
        else
          fequal ((-. b *. dy) /. (a *. dx)) 1.0
      in
      if parallel then (* special case when segment & line are parallel *)
        if Line3.contains_point (a, b, c) (x1, y1) then
          Some (x1, y1) (* coincident *)
        else
          None (* non-coincident *)
      else
        let u = (c -. b *. y1 -. a *. x1) /. (a *. dx +. b *. dy) in
        if fequal u 0.0 then
          Some (x1, y1)
        else if fequal u 1.0 then
          Some (x2, y2)
        else if u >= 0.0 && u <= 1.0 then
          Some (x1 +. u *. dx, y1 +. u *. dy)
        else
          None (* outside this segment *)

end

module Circle = struct

  type t = { center: Point.t; radius: float; }

  let as_string c =
    "Circle (center: " ^ (Point.as_string c.center)
      ^ "; radius: " ^ (string_of_float c.radius) ^ ")"

end

