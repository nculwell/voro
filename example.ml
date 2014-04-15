
open Geom
open Printf

let sites = [
  (1.0, 1.0);
  (2.0, 1.0);
  (1.0, 2.0);
  (2.0, 2.0);
]

let polys = Voro.cells_make 0.0 (3.0, 3.0) sites

let () =
  polys |> List.iter (fun poly -> print_endline (Polygon.as_string poly));
  ()

