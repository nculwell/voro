
open Geom

module A = BatArray
module L = BatList
module V = BatVector

module Site = struct
  type t = Point.t * Polygon.t
end

module Cell = struct
  type t = Site.t * HalfEdge.t V.t * bool
end



(* TODO
(* Grid used to organize points so that they can be found. *)
type grid = Point.t list array array

let grid_of_points
    (points: Point.t list)
    (sz_x: int)
    (sz_y: int)
    (density: int)
    : grid =
  A.make 
*)

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
  let rec aux ptsL ptsR =
    match ptsR with
    | [] -> (* TODO *)
    | pt :: rest ->

