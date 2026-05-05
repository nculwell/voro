
module A = BatArray
module L = BatList
open Geom

let draw_poly ctx (poly: Polygon.t) =
  match A.to_list poly with
  | [] -> ()
  | (x, y) :: rest ->
      Cairo.Path.clear ctx;
      Cairo.move_to ctx x y;
      L.iter (fun (x, y) -> Cairo.line_to ctx x y) rest;
      Cairo.Path.close ctx;
      Cairo.set_source_rgb ctx (Random.float 1.0)
                               (Random.float 1.0)
                               (Random.float 1.0);
      (* Cairo.stroke ctx; *)
      Cairo.fill ctx

let draw_polys ((w, h): Size.t)
               (polys: Polygon.t list)
               (filename: string)
               : unit =
  let width, height = (int_of_float w, int_of_float h) in
  let surface =
    Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:height in
  let ctx = Cairo.create surface in
  Cairo.set_line_width ctx 1.;
  L.iter (draw_poly ctx) polys;
  Cairo.PNG.write surface (filename ^ ".png")

