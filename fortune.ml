
open Geom

module A = BatArray
module L = BatList
(* module V = BatVector *)

module Site = struct
  type t = Point.t * Polygon.t
end

module Cell = struct
  type t = Site.t * HalfEdge.t V.t * bool
end

module Edge = struct
  (* (a, b, c), (ep 2) (reg 2) *)
  type t =
    Line3.t
    * (Point.t * Point.t) option
    * (Point.t * Point.t)
end

module HalfEdge = struct
  type t = {
    left: t;
    right: t;
    edge: Edge.t option;
    pm: LR.t;
    vertex: Point.t;
    ystar: float;
    next: t;
  }
end

let bisect ((x1, y1) as s1: Point.t) ((x2, y2) as s2: Point.t): Edge.t =
  let dx, dy = x2 - x1, y2 - y1 in
  let c = x1 *. dx + y1 *. dy + (dx *. dx + dy *. dy) /. 2 in
  let line =
    if float_abs dx > float_abs dy then
      (1.0, dy /. dx, c /. dx)
    else
      (1.0, dx /. dy, c /. dy)
  in
  line None (s1, s2)

let intersect (el1: HalfEdge.t) (el2: HalfEdge.t): Point.t option =
  match el1.edge, el2.edge with
  | None, _ | _, None -> None
  | Some (_, _, (_, r1)), Some (_, _, (_, r2)) when r1 = r2 -> None
  | Some (((a1, b1, c1), ep1, (_, r1)) as e1),
    Some (((a2, b2, c2), ep2, (_, r2)) as e2) ->
      let d = a1 *. b2 -. b1 *. a2 in
      if fequal d 0.0 then
        None
      else
        let x_intersect = (c1 *. b2 -. c2 *. b1) /. d in
        let y_intersect = (c2 *. b1 -. c1 *. b2) /. d in
        let r1x, r1y = r1 in
        let r2x, r2y = r2 in
        let el, e =
          if r1y < r2y || (r1y = r2y && r1x < r2x) then
            el1, e1
          else
            el2, e2
        in
        let right_of_site = x_intersect >= e1x in
        if (right_of_site && el1.pm == Left)
            || (!right_of_site && el1.pm == Right) then
          None
        else
          Some (x_intersect, y_intersect)

let list_minmax (eval: 'a -> 'b) (lst: 'a list): ('a * 'a) =
  match lst with
  | [] -> raise Not_found
  | first :: rest ->
      let (min, _, max, _) =
        rest |>
          L.fold_left
          (fun (min, minval, max, maxval) e ->
            let v = eval e in
            let new_min, new_minval =
              if compare v minval < 0 then (e, v) else (min, minval)
            in
            let new_max, new_maxval = 
              if compare v maxval > 0 then (e, v) else (max, maxval)
            in
            (new_min, new_minval, new_max, new_maxval))
          (first, eval first, first, eval first)
      in
      (min, max)

(* Grid used to organize points so that they can be found. *)
type grid = float * Point.t list array array

let calc_cell_size (w, h) n_cells: float =
  sqrt (w *. h /. float n_cells)

let grid_cell_of_point grd pt =
  let (cell_w, _) = grd in
  let (px, py) = pt in
  let cx = ifloor (x /. cell_w) in
  let cy = ifloor (y /. cell_w) in
  (cx, cy)

let grid_of_points
    (points: Point.t list)
    (size: float * float)
    (density: int)
    : grid =
  let w, h = size in
  let n = L.length points in
  let cell_w = calc_cell_size size n in
  let gw = ceil (w /. cell_w) |> int_of_float in
  let gh = ceil (h /. cell_w) |> int_of_float in
  let grd = A.init gw (fun _ -> A.make gh []) in
  points |> List.iter
    (fun pt ->
      let (cell_x, cell_y) = grid_cell_of_point (cell_w, _) pt in
      grd.(cell_x).(cell_y) <- pt :: grd.(cell_x).(cell_y));
  (cell_w, grd)

let nearest_by_grid grd (x, y) =
  let cell_x, cell_y = grid_cell_of_point grd (x, y) in
  let gw, gh = A.length grd, A.length grd.(0) in

  (* TODO *)

(* Test *)
let () =
  grid_of_points [
    (0., 0.); (1., 0.); (2., 0.); (3., 0.)

(* Find the nearest point to a given point (excluding itself). *)
(* TODO: use grid *)
let nearest points (x, y) =
  let rec aux pts closest min_dist =
    match pts with
    | [] -> closest
    | pt :: rest when pt = (x, y) ->
        (* Don't compare this point to itself *)
        aux rest closest min_dist
    | pt :: rest ->
        let dist = Point.distance pt (x, y) in
        if dist < min_dist || min_dist = -1 then
          aux rest pt dist
        else
          aux rest closest min_dist
  in
  aux points (-1, -1) -1

let star points (x, y) =
  (x, y + Point.distance (nearest points (x, y), (x, y)))

let fortune (points: Point.t list): Site.t list =
  (* Sort points by y, then by x. *)
  let cmp (a, b) (c, d) = compare (b, a) (d, c) in
  let sorted_points = L.sort ~cmp points in
  let xcoords = L.map (fun (x, _) -> x) sorted_points in
  let xmin = L.min xcoords in
  let xmax = L.max xcoords in
  let (_, ymin) = L.hd sorted_points in
  let (_, ymax) = L.last sorted_points in
  let delta_x, delta_y = xmax - xmin, ymax - ymin in
  let rec aux ptsL ptsR =
    match ptsR with
    | [] -> (* TODO *)
    | pt :: rest ->



void
geominit(void)
    {
    freeinit(&efl, sizeof(Edge)) ;
    nvertices = nedges = 0 ;
    sqrt_nsites = sqrt(nsites+4) ;
    deltay = ymax - ymin ;
    deltax = xmax - xmin ;
    }

