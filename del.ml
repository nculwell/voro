
open Geom

module LR = struct

  type t = Left | Right

  let other x = match x with
    | Left -> Right
    | Right -> Left

  let as_string x = match x with
    | Left -> "Left"
    | Right -> "Right"

end

module Site = struct

  type t = {
    coord: Point.t;
    color: int;
    weight: float;
    index: int;
    edges: Edge.t list ref;
    region: Point.t list ref;
  }

  let create (p, index, weight, color) =
    { coord = p; index = index; weight = weight = weight; color = color; }

end

module Edge = struct

  (* The line segment connecting the two Sites is part of the Delaunay
   * triangulation; the line segment connecting the two Vertices is part of the
   * Voronoi diagram *)

  type t = float * float * float
  (*
  private static var _nedges:int = 0;
  internal var a:Number, b:Number, c:Number;
  private var _leftVertex:Vertex;
  *)

  let create_bisecting_edge (site1: Site.t, site2: Site.t): t =
    let diff = Point.sub 
end

module HalfEdge = struct

  type t = {
    edge_list_l_neighbor: t;
    edge_list_r_neighbor: t;
    next_in_prio_queue: t;
    edge: Edge.t;
    left_right: LR.t;
    vertex: Vertex.t;
  }

  (* TODO *)

end

