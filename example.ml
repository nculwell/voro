
open Geom
open Printf

let sites = [
  (1.0, 1.0);
  (2.0, 1.0);
  (1.0, 2.0);
  (2.0, 2.0);
]

let polys = Voro.cells_make 0.0 (3.0, 3.0) sites

let print_polys polys =
  polys |> List.iter (fun poly -> print_endline (Polygon.as_string poly))

let random_points (size: Size.t) (n: int): Point.t list =
  let w, h = size in
  L.init n (fun _ -> (Random.float w, Random.float h))

let random_example size n filename =
  let sites = random_points size n in
  let polys = Voro.cells_make 0.0 size sites in
  print_polys polys;
  Draw.draw_polys size polys filename

let () =
  random_example (800., 600.) 100 "output/ex"

