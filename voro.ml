
(* Copyright (c) 2014 Nathan Culwell-Kanarek
 * Licensed under the MIT license:
 * http://www.opensource.org/licenses/mit-license.php
 *)

(* Original copyright:
 * Copyright (c) 2011 Psellos http://psellos.com
 * Licensed under the MIT license:
 * http://www.opensource.org/licenses/mit-license.php
 *)

open Geom

(* Process the next edge of a polygon, tracking chains on the two sides of the
 * given line. If the line intersects the edge at m, end the current chain at m
 * and start a new chain at m. If it intersects the middle of the edge, split
 * the edge at m first. If there's no intersection, just add to current chain.
*)
let edge_add chains line p q =
  let to_cur chains p =
    match chains with
    | [] -> [[p]] (* Doesn't actually come up *)
    | c :: rest -> (p :: c) :: rest
  in let to_new chains p =
    [p] :: chains
  in let chains' = to_cur chains p (* Always goes onto current chain *)
  in
    match line_seg_intersect line p q with
    | None -> chains'
    | Some m ->
        if ptequal m p then
          if ptonline line q then
            (* Line coincident with edge. Doesn't count as intersection. *)
            chains'
          else
            (* Intersects at vertex, start new chain. *)
            to_new chains' p
          else if ptequal m q then
            (* Treat edges as half open; i.e., handle vertex q as part of next
             * edge. *)
            chains'
        else
          (* Split at intersection point. *)
          to_new (to_cur chains' m) m

(* The line might split the given convex polygon into two parts, in which case
 * return them. Otherwise return the original polygon and an empty polygon.
*)
let poly_line_split poly line : Polygon.t * Polygon.t =
    match poly with
    | [] -> ([], [])
    | p0 :: _ ->
        let rec findchains chains pts =
          match pts with
          | [] ->
              List.rev_map List.rev chains
          | p :: [] ->
              findchains (edge_add chains line p p0) []
          | p :: (q :: _ as rest) ->
              findchains (edge_add chains line p q) rest
        in
        match findchains [[]] poly with
        | [s0; s1; s2] -> (s0 @ s2, s1)
        | _ -> (poly, [])

(* Intersect the given convex polygon and the given half plane, giving a new
 * convex polygon (or possibly an empty one).
*)
let poly_hplane_intersect poly hplane : poly =
  let (p1, p2) =
    match poly_line_split poly hplane with
    | ([], p) -> (p, []) (* Test the nonempty one if there is one *)
    | p1p2 -> p1p2
  in
  if polyinhplane hplane p1 then p1 else p2

(* Return the halfplane containing (a, b), with its boundary halfway between
 * (a, b) and (c, d).
*)
let site_site_hplane (a, b) (c, d) =
  let flip (a, b, c) = (-. a, -. b, -. c) in
  let hplane =
    (2.0 *. (c -. a),
     2.0 *. (d -. b),
     c *. c -. a *. a +. d *. d -. b *. b)
  in
  if ptinhplane hplane (a, b) then hplane else flip hplane

(* Site is a Voronoi site with the given cell as calculated so far.  * Add an
edge to the cell for the given external site and return * the new cell. If
xsite isn't close enough to site, it won't * affect the cell shape (no new edge
will be added). If xsite is * the same as site, just return the cell.  *)
let site_add_edge site cell xsite : poly =
  if ptequal site xsite then
    cell
  else
    poly_hplane_intersect cell (site_site_hplane site xsite)

(* Calculate the cells for the given Voronoi sites. Return a list of polygons
 * corresponding to the cells. *)
let cells_make 
    (size: Size.t)
    (sites: Point.t list)
    : Point.t list list =
  let margin = 10.0 in
  let (w, h) = size in
  let screen = [(-. margin, -. margin); (wd +. margin, -. margin);
     (wd +. margin, ht +. margin); (-. margin, ht +. margin)]
  in
  let make1 site =
    List.fold_left (site_add_edge site) screen sites
  in
  List.map make1 sites

