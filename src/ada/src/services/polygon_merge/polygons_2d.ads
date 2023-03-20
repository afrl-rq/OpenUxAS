-- -----------------------------------------------------------------------------
-- polygons_2d.ads              Dependable Computing
-- Corresponds to logic from polygons_2d.pvs
-- -----------------------------------------------------------------------------
with prelude;
with vectors_2d;
with vectors_cross_2d;
with segments_2d;
with vertex_list;

package polygons_2d with SPARK_Mode is
  use prelude;
  use vectors_2d;
  use vectors_cross_2d;
  use segments_2d;
  use vertex_list;
  -- Require that the number of vertices be above 2.
  subtype vertex_count_type is Positive range 3 .. 100;

  -- TODO: change to upfrom
  function polygon_2d_constraint(uvl: uniq_vertex_list) return Boolean is
    ((uvl.num_vertices > 2) and then
       -- Retrenchment below
       (for all i in 0 .. uvl.num_vertices-2 =>
            segment_2d_constraint(uvl.vertices(i), uvl.vertices(i+1))
       ) and then 
         (
          segment_2d_constraint(uvl.vertices(uvl.num_vertices-1),
            uvl.vertices(0))
         ) and then
       (for all i in 0 .. uvl.num_vertices-1 =>
            (for all j in 0 .. uvl.num_vertices-1 =>
                 (can_subtract(uvl.vertices(j), uvl.vertices(i)) and then
                  can_add(uvl.vertices(i), 
                    vector_from_point_to_point(uvl.vertices(i), 
                      uvl.vertices(j))))
            )
       ));

   -- Represent a 2D polygon without holes by its vertices, which are assumed
   -- to be given in CCW order.
   -- From polygons_2d.pvs:
   --   polygon_2d: NONEMPTY_TYPE =
   --    [#
   --      num_vertices: upfrom(2),
   --      vertices: uniq_vertex_list(num_vertices)
   --     #];
   subtype polygon_2d is uniq_vertex_list with
    Predicate =>
      (uniq_vertex_list_pred(polygon_2d.num_vertices, 
       polygon_2d.vertices)) and then
    (polygon_2d_constraint(polygon_2d));

  -- From polygons_2d.pvs
  --  prev_index(p: polygon_2d, cur_index: below(p`num_vertices)):
  --      below(p`num_vertices) =
  --    IF cur_index = 0 THEN
  --      p`num_vertices - 1
  --    ELSE
  --      cur_index - 1
  --    ENDIF;
  function prev_index(p: polygon_2d; cur_index: nat)
                      return nat is
    (if (cur_index = 0) 
     then
       p.num_vertices - 1
     else
       cur_index - 1
    )
      with Pre => below(p.num_vertices, cur_index),
      Post => below(p.num_vertices, prev_index'Result);
  
  -- From polygons_2d.pvs:
  --   next_index(p: polygon_2d, cur_index: below(p`num_vertices)):
  --       below(p`num_vertices) =
  --     IF cur_index = p`num_vertices - 1 THEN
  --       0
  --     ELSE
  --       cur_index + 1
  --     ENDIF;
  --NB: Weakened type of p to allow for loop invariants and asserts
  -- to use this code before a polygon is properly created
  function next_index(p: bounded_vertex_list; cur_index: nat)
                       return nat is
    (if (cur_index = p.num_vertices - 1) then
       0
     else
       cur_index + 1
    )
      with Pre => below(p.num_vertices, cur_index),
      Post => below(p.num_vertices, next_index'Result);

   -- From polygons_2d.pvs:
   --  % Curried predicate useful for creating types
   --  vertex?(polygon: polygon_2d): pred[point_2d] =
   --    LAMBDA(p: point_2d):
   --      EXISTS(i: below(polygon`num_vertices)):
   --        polygon`vertices(i) = p;
   function is_vertex(polygon: polygon_2d; p: point_2d) return Boolean is
     (for some i in 0 .. polygon.num_vertices-1 =>
        polygon.vertices(i) = p);

   -- From polygons_2d.pvs:
   --   edges_of_polygon(polygon: polygon_2d):
   --       [below(polygon`num_vertices) -> segment_2d] =
   --     LAMBDA(i: below(polygon`num_vertices)):
   --       (# p1 := polygon`vertices(i),
   --          p2 := polygon`vertices(next_index(polygon, i)) #);
   --NB: Weakened types of inputs and outputs so that this can be used in
   -- loop invariants and asserts prior to having proper polygons.
   function weak_edges_of_polygon(polygon: bounded_vertex_list; i: nat) return weak_segment_2d is
     ((p1 => polygon.vertices(i),
       p2 => polygon.vertices(next_index(polygon, i))))
     with
      Pre => (below(polygon.num_vertices, i)),
      Post => True;
   -- This is the version with the correct types
   function edges_of_polygon(polygon: polygon_2d; i: nat) return segment_2d is
     ((p1 => polygon.vertices(i),
       p2 => polygon.vertices(next_index(polygon, i))))
     with
      Pre => (below(polygon.num_vertices, i)),
      Post => True;
   
   -- From polygons_2d.pvs:
   --   % Curried predicate useful for creating types
   --   edge_of_polygon?(polygon: polygon_2d): pred[segment_2d] =
   --     LAMBDA(s: segment_2d):
   --       EXISTS(i: below(polygon`num_vertices)):
   --         edges_of_polygon(polygon)(i) = s;
   function is_edge_of_polygon(polygon: polygon_2d; s: segment_2d) return Boolean is
     (for some i in 0 .. polygon.num_vertices-1 =>
         edges_of_polygon(polygon, i) = s);

   -- From polygons_2d.pvs:
   --   % idx being of type below(p`num_vertices) implicitly requires p to have
   --   % a non-zero number of vertices
   --   find_helper(p: polygon_2d, idx: below(p`num_vertices), point: point_2d):
   --       RECURSIVE {i: int | i >= -1 AND i < p`num_vertices} =
   --     % Terminating condition when found
   --     IF p`vertices(idx) = point THEN
   --       idx
   --     % Terminating condition when not found
   --     ELSIF idx = p`num_vertices - 1 THEN
   --       -1
   --     ELSE
   --     % Look at the next point
   --       find_helper(p, idx + 1, point)
   --     ENDIF
   --     MEASURE p`num_vertices - 1 - idx;
   --   % Find index value of vertex matching point or return -1 if there's no match
   --   find_index(polygon: polygon_2d, point: point_2d):
   --     {i: int | (i = -1 AND NOT
   --                  EXISTS(v: (vertex?(polygon))): v = point) OR
   --               (i >= 0 AND i < polygon`num_vertices AND
   --                 polygon`vertices(i) = point)} =
   --     IF polygon`num_vertices = 0 THEN
   --       -1
   --     ELSE
   --       find_helper(polygon, 0, point)
   --     ENDIF;
   function find_index(polygon: polygon_2d; point: point_2d) return integer with
     Post =>
       ((find_index'Result = -1 and then not is_vertex(polygon, point)) or else
          (find_index'Result >= 0 and then find_index'Result < polygon.num_vertices and then
                 polygon.vertices(find_index'Result) = point));

  --  equal_or_adjacent_edge?(n: nonneg_int, i, j: below(n)): bool =
  --    % Simple adjacency
  --    (abs(i - j) <= 1) OR
  --    % Wrapping adjacency
  --    (i = n-1 AND j = 0) OR (i = 0 AND j = n-1);
  function equal_or_adjacent_edge(n, i, j: nat) return Boolean is
    ((abs(i - j) <= 1) OR
       (i = n-1 AND j= 0) OR (i = 0 and j = n-1))
      with Pre =>
        below(n, i) AND below(n, j);

  --  polygon_edges_do_not_cross?(p: polygon_2d): bool =
  --    FORALL (i, j: below(p`num_vertices)):
  --      LET ei = edges_of_polygon(p)(i), ej = edges_of_polygon(p)(j) IN
  --        FORALL (q: (is_point_on_segment?(ei))):
  --          is_point_on_segment?(ej)(q) IMPLIES
  --            i = j OR q = ei`p1 OR q = ei`p2
  --...
  --  polygon_edges_do_not_cross?_alt(p: polygon_2d): bool =
  --  FORALL(i: below(p`num_vertices),
  --         j: {n: below(p`num_vertices) | n /= i}):
  --    LET ei = edges_of_polygon(p)(i), ej = edges_of_polygon(p)(j) IN
  --    IF equal_or_adjacent_edge?(p`num_vertices, i, j) THEN
  --      NOT ^(ei) = -^(ej)
  --    ELSE
  --      NOT are_segments_intersecting?(ei)(ej)
  --    ENDIF;
  --...
  --PROVEN LEMMA:
  --  polygon_edges_do_not_cross_defs_eq: LEMMA
  --    FORALL(p: polygon_2d):
  --      polygon_edges_do_not_cross?(p) IFF
  --        polygon_edges_do_not_cross?_alt(p)
  function polygon_edges_do_not_cross(p: polygon_2d) return Boolean is
    (for all i in 0 .. p.num_vertices-1 =>
       (for all j in 0 .. p.num_vertices-1 =>
            (i = j) or -- Corresponds to n /= i in PVS spec
            (if equal_or_adjacent_edge(p.num_vertices, i, j) then
                   not (normalize(edges_of_polygon(p, i)) =
               -normalize(edges_of_polygon(p, j)))
             else
               not are_segments_intersecting(edges_of_polygon(p, i),
                 edges_of_polygon(p, j)))))
      with Post=>True;

   -- From polygons_2d.pvs:
   --   point_on_polygon_perimeter?(G: polygon_2d)(p: point_2d): bool =
   --     EXISTS (e: (edge_of_polygon?(G))):
   --       point_on_segment?(p, e)
   -- We cannot iterate over the edge_of_polygon predicate in SPARK Ada, so
   -- instead we iterate over the edges directly
   function is_point_on_polygon_perimeter(G: polygon_2d; p: point_2d) return Boolean is
     (for some i in 0 .. G.num_vertices-1 =>
         is_point_on_segment(p, edges_of_polygon(G, i)))
     with Pre =>
       (for all i in 0 .. G.num_vertices-1 =>
          can_subtract(p, G.vertices(i)));

  --  % Copied from
  --  % http://web.archive.org/web/20120323102807/http://local.wasp.uwa.edu.au/~pbourke/geometry/insidepoly/
  --  x_in_range(p: point_2d, p1: point_2d, p2: point_2d): bool =
  --    IF (p2`y - p1`y < 0) THEN
  --      (p2`y - p1`y) * (p`x - p1`x) > (p2`x - p1`x) * (p`y - p1`y)
  --    ELSE
  --      (p2`y - p1`y) * (p`x - p1`x) < (p2`x - p1`x) * (p`y - p1`y)
  --    ENDIF;
  function x_in_range(p, p1, p2: point_2d) return Boolean is
    (if (p2.y - p1.y < 0.0) then
         (p2.y - p1.y) * (p.x - p1.x) > (p2.x - p1.x) * (p.y - p1.y)
     else
       (p2.y - p1.y) * (p.x - p1.x) < (p2.x - p1.x) * (p.y - p1.y)
    )
      with Pre =>
        can_subtract(p2, p1) and can_subtract(p, p1);

  --  % Determine if the y coordinate is between y1 and y2, checking both orders.
  --  y_in_range(p: point_2d, p1: point_2d, p2: point_2d): bool =
  --    (p1`y <= p`y AND p`y < p2`y) OR
  --    (p2`y <= p`y AND p`y < p1`y);
  function y_in_range(p, p1, p2: point_2d) return Boolean is
    ((p1.y <= p.y and p.y < p2.y) or
         (p2.y <= p.y and p.y < p1.y));

  --  is_right_point_ray_intersection?(point: point_2d, segment: segment_2d): bool =
  --    y_in_range(point, segment`p1, segment`p2) AND
  --    x_in_range(point, segment`p1, segment`p2);
  function is_right_point_ray_intersection(point: point_2d; segment: segment_2d) return Boolean is
    (y_in_range(point, segment.p1, segment.p2) and
         x_in_range(point, segment.p1, segment.p2))
      with Pre =>
        can_subtract(point, segment.p1);

  --  is_point_in_polygon_recursive?(polygon: polygon_2d, point: point_2d,
  --                                index: below(polygon`num_vertices),
  --                                is_inside: bool): RECURSIVE bool =
  --    LET segment: segment_2d = edges_of_polygon(polygon)(index) IN
  --    IF (index = polygon`num_vertices - 1) THEN
  --      is_inside XOR is_right_point_ray_intersection?(point, segment)
  --    ELSE
  --      is_point_in_polygon_recursive?(polygon, point, index + 1,
  --              is_inside XOR is_right_point_ray_intersection?(point, segment))
  --    ENDIF
  --   MEASURE (polygon`num_vertices - index);
  --...
  --  % Is the point inside the polygon, but not on the edge of the polygon?
  --  is_point_in_polygon_exclusive?(polygon: polygon_2d)(point: point_2d): bool =
  --    IF (polygon`num_vertices < 3) THEN
  --      FALSE
  --    ELSIF is_point_on_polygon_perimeter?(polygon)(point) THEN
  --      FALSE
  --    ELSE
  --      is_point_in_polygon_recursive?(polygon, point, 0, FALSE)
  --    ENDIF;
  --NOTE: Changing recursive function into iteration
  function is_point_in_polygon_exclusive(polygon: polygon_2d; point: point_2d) return Boolean
    with Pre =>
      (for all i in 0 .. polygon.num_vertices-1 =>
         can_subtract(point, polygon.vertices(i)));

  --  % A simple polygon has to
  --  % 1. Have at least 3 vertices (and hence 3 edges)
  --  % 2. No edges cross other edges
  --  % 3. No vertices lie on other edges
  --  % polygon_edges_do_not_cross handles both 2 & 3
  --  simple_polygon_2d?(p: polygon_2d): bool =
  --    p`num_vertices >= 3 AND
  --    polygon_edges_do_not_cross?(p);
  --...
  --  simple_polygon_2d: NONEMPTY_TYPE = (simple_polygon_2d?)
  --   CONTAINING example_right_triangle;
  subtype simple_polygon_2d is polygon_2d with
    Predicate => 
    (uniq_vertex_list_pred(simple_polygon_2d.num_vertices, 
                           simple_polygon_2d.vertices)) and then
    (polygon_2d_constraint(simple_polygon_2d));
end polygons_2d;
