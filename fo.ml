
module Cell = struct
  type t = Point.t * Polygon.t
end

module CircleEvent = struct
  (* Three points for the relevant sites, one for the bottom point of the
   * circle. *)
  type t = (Point.t * Point.t * Point.t) * Point.t
end

let rec process_events (site_events: Point.t list)
                       (circle_events: CircleEvent.t list)
                       (site_queue: (Point.t * Point.t option) list)
                       (vertices: Point.t list)
                       : Cell.t list * CircleEvent.t list =
  match site_events with
  | [] -> (vertices, circle_events)
  | (_, site_y) as site_evt :: site_evts ->
      match circle_events with
      | (_, (_, bottom_y)) as c_evt :: c_evts when bottom_y < site_y ->
          process_circle_event c_evt site_events c_evts site_queue vertices
      | _ ->
          let new_site_queue = insert_site site_evt site_queue in
          process_events site_evts circle_events new_site_queue vertices

and insert_site ((site_x, site_y) as site) site_queue =
  match site_queue with
  | [] -> site :: []
  | (sx, sy) as s :: ss ->
      if sx < site_x then
        site :: site_queue
      else
        s :: insert_site site ss

and process_circle_event (site_events: Point.t list)
                         (circle_events: CircleEvent.t list)
                         (site_queue: Point.t list)
                         (vertices: Point.t list)
                         : Cell.t list * CircleEvent.t list =
  match 

let sort_points points =
  let cmp (a, b) (c, d) = compare (b, a) (d, c) in
  let sorted_points = L.sort ~cmp points in
  sorted_points

let get_min_max sorted_points =
  let xcoords = L.map (fun (x, _) -> x) sorted_points in
  let xmin = L.min xcoords in
  let xmax = L.max xcoords in
  let (_, ymin) = L.hd sorted_points in
  let (_, ymax) = L.last sorted_points in
  (xmin, xmax, ymin, ymax)

let fortune ((bound_x, bound_y), (bound_w, bound_h): Rect.t)
            (points: Point.t list)
            : Cell.t list =
  if points = [] then [] else
  let sorted_points = sort_points points in
  let xmin, xmax, ymin, ymax = get_min_max sorted_points in
  (* let delta_x, delta_y = xmax - xmin, ymax - ymin in *)
  process_events 

for each site
   {
      create a site event e, 
      e.point = current site, insert e into queue
   }
   while queue is not empty
   {
      e = get the first event from the queue
      if its site event : AddParabola( e.point )
      else : RemoveParabola( e.parabola );
   }
   // done!!! :) 


struct event {
   double x;
   point p;
   arc *a;
   bool valid;
   function AddParabola ( point u )
   {
      par = arc under point u;
      if (par has its circle event, when it is removed form the beachline)
         remove this event form the queue
      new arcs a, b, c;
      b.site = u;
      a.site = c.site = par.site; // site of arc is a focus of arc
      xl, xr  = left and right edges, which comes from point on par under u
      xl is a normal to  (a.site, b.site);
      xr is a normal to (b.site, c.site);
      replace par by the sequence a, xl, b, xr, c
      CheckCircleEvent(a);
      CheckCircleEvent(c);
   }
   function RemoveParabola ( Parabola p )
   {
      l = an arc lef to p;
      r = an arc on the right from p;
      if (l or r have their Circle events) remove these events from the queue
      s = the circumcenter between l.site, p.site and r.site
      x = new edge, starts at s, normal to (l.site, r.site)
      finish two neighbour edges xl, xr at point s
      replace a sequence xl, p, xr by new edge x
      CheckCircleEvent(l);
      CheckCircleEvent(r);
   }
   function CheckCircleEvent(Parabola p)
   {
      l = arc on the left to p;
      r = arc on the right to p;
      xl, xr = edges by the p
      when there is no l  OR no r  OR  l.site=r.site  RETURN
      s = middle point, where xl and xr cross each other
      when there is no s (edges go like\ /) RETURN
      r = distance between s an p.site (radius of curcumcircle)
      if s.y + r is still under the sweepline  RETURN
      e = new circle event
      e.parabola = p;
      e.y = s.y + r;
      add e into queue 
   }

