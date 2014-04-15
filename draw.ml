
module A = BatArray
module L = BatList
open Geom

let draw_poly ctx (poly: Polygon.t) =
  match A.to_list poly with
  | [] -> ()
  | (x, y) :: rest ->
      Cairo.new_path ctx;
      Cairo.move_to ctx ~x ~y;
      L.iter (fun (x, y) -> Cairo.line_to ctx ~x ~y) rest;
      Cairo.close_path ctx;
      Cairo.set_source_rgb ctx ~red:(Random.float 1.0)
                               ~green:(Random.float 1.0)
                               ~blue:(Random.float 1.0);
      (* Cairo.stroke ctx; *)
      Cairo.fill ctx

let draw_polys ((w, h): Size.t)
               (polys: Polygon.t list)
               (filename: string)
               : unit =
  let width, height = (int_of_float w, int_of_float h) in
  let surface =
    Cairo.image_surface_create Cairo.FORMAT_ARGB32 ~width ~height in
  let ctx = Cairo.create surface in
  Cairo.set_line_width ctx 1.;
  L.iter (draw_poly ctx) polys;
  Cairo_png.surface_write_to_file surface (filename ^ ".png")

