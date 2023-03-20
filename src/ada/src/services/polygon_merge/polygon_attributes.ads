-- -----------------------------------------------------------------------------
-- polygon_attributes.ads              Dependable Computing
-- Corresponds to logic from polygon_attributes.pvs
-- -----------------------------------------------------------------------------
with polygons_2d;
with vectors_2d;
with segments_2d;
with between_rays;
with lemmas;
with prelude;
with topleft_vertex;

package polygon_attributes with SPARK_Mode is
  use polygons_2d;
  use vectors_2d;
  use segments_2d;
  use between_rays;
  use lemmas;
  use prelude;
  use topleft_vertex;

  --   % To determine whether a point lies between the rays induced by two
  --   % edges, we differentiate the inward and outward cases.  The inward
  --   % case uses the same ccw logic after first swapping the edges and
  --   % their endpoints.
  --   point_between_edges?(outward: bool, G: simple_polygon_2d,
  --                        i: below(G`num_vertices))(p: point_2d): bool =
  --     LET s = edges_of_polygon(G)(prev_index(G, i)),
  --         e = edges_of_polygon(G)(i)
  --     IN IF outward THEN between_rays?(s, e)(p)
  --                   ELSE between_rays?(reverse_segment(e), reverse_segment(s))(p)
  --        ENDIF
  function point_between_edges(outward: Boolean; G: simple_polygon_2d;
                               i: Natural; p: point_2d) return Boolean
    with Pre =>
      below(G.num_vertices, i) and then
      (for all i in 0 .. G.num_vertices-1 =>
         can_subtract(G.vertices(i), p) and can_subtract(p, G.vertices(i))),
      Post =>
        point_between_edges'Result =
        (if outward then
           is_between_rays(edges_of_polygon(G,  prev_index(G, i)),
             edges_of_polygon(G, i), p)
             else
               is_between_rays(reverse_segment(edges_of_polygon(G, i)),
           reverse_segment(edges_of_polygon(G, prev_index(G, i))), p));

  --  ccw_vertex_order?(G: simple_polygon_2d): bool =
  --    LET i = topleft_vertex_idx(G),
  --        r = (# x := 1, y := 0 #)
  --    IN point_between_edges?(true, G, i)(G`vertices(i) - r)
  function ccw_vertex_order(G: simple_polygon_2d) return Boolean is
    (point_between_edges(true, G, topleft_vertex_idx(G),
     G.vertices(topleft_vertex_idx(G)) - (x => 1.0, y => 0.0)))
    with Pre =>
      (can_subtract(G.vertices(topleft_vertex_idx(G)), (x => 1.0, y => 0.0))) and then
      (for all i in 0 .. G.num_vertices-1 =>
         can_subtract(G.vertices(i), G.vertices(topleft_vertex_idx(G)) - (x => 1.0, y => 0.0)) and
           can_subtract(G.vertices(topleft_vertex_idx(G)) - (x => 1.0, y => 0.0), G.vertices(i)));

end polygon_attributes;
