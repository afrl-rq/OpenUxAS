-- -----------------------------------------------------------------------------
-- perimeter_props.ads               Dependable Computing
-- Corresponds to logic from perimeter_props.pvs
-- -----------------------------------------------------------------------------
-- For segments to be used in visibility graphs that have epsilon constraints
with eps_segments;
with vectors_2d;
with vertex_list;
with polygons_2d;
with prelude;
with horiz_edge_crossing;

package perimeter_props with SPARK_Mode is
  use eps_segments;
  use vectors_2d;
  use vertex_list;
  use polygons_2d;
  use prelude;
  use horiz_edge_crossing;

  -- PVS:
  --  push_back(vtx_list: finseq[point], p: point): finseq[point] =
  --    (# length := vtx_list`length + 1,
  --       seq    := LAMBDA(i: upto(vtx_list`length)):
  --                   IF i < vtx_list`length THEN
  --                     vtx_list`seq(i)
  --                   ELSE
  --                     p
  --                   ENDIF #);
  function push_back(vtx_list: bounded_vertex_list; p: point) return bounded_vertex_list is
    ((num_vertices => vtx_list.num_vertices + 1,
      vertices => vtx_list.vertices(0..vtx_list.num_vertices) & p & vtx_list.vertices(vtx_list.num_vertices+1..MAX_NUM_VERTICES-1)))
  with
    Pre => vtx_list.num_vertices < MAX_NUM_VERTICES;

  --  % Corresponds to visilibity.cpp for loop in function
  --  %   double Polygon::area() const
  --  area_helper(vertices: finseq[point], i: upto(vertices`length)):
  --      RECURSIVE real =
  --    IF i = vertices`length THEN 0
  --    ELSE
  --      vertices(i)`x * modget(vertices, i+1)`y -
  --        modget(vertices, i+1)`x * vertices(i)`y +
  --        area_helper(vertices, i+1)
  --    ENDIF
  --    MEASURE vertices`length - i;
  --
  --  % Corresponds to visilibity.cpp function
  --  %   double Polygon::area() const
  --  area(vertices: finseq[point]): real =
  --    area_helper(vertices, 0) / 2;
  function area(vertices: bounded_vertex_list) return Float;

  --  projection_onto_boundary_of_recurse(p: point_2d, polygon_temp: polygon_2d,
  --      vtx_idx: upto(polygon_temp`num_vertices),
  --      running_projection: point_2d, running_min: real): RECURSIVE point_2d =
  --    IF vtx_idx = polygon_temp`num_vertices THEN
  --      running_projection
  --    ELSE
  --      LET pointemp = projection_onto(p, edges_of_polygon(polygon_temp)(vtx_idx)),
  --          new_dist = distance(p, pointemp),
  --          new_proj = IF new_dist < running_min THEN pointemp ELSE running_projection ENDIF,
  --          new_min  = IF new_dist < running_min THEN new_dist ELSE running_min ENDIF IN
  --      projection_onto_boundary_of_recurse(p, polygon_temp, vtx_idx+1, new_proj, new_min)
  --    ENDIF
  --    MEASURE polygon_temp`num_vertices - vtx_idx;
  --
  --  % Generalization of function below to enable use of induction scheme
  --  % backward_below_induction.
  --  projection_onto_boundary_of_gen(p: point_2d,
  --                                  polygon_temp: polygon_2d,
  --                                  k: upto(polygon_temp`num_vertices)): point_2d =
  --    LET running_projection =
  --          polygon_temp`vertices(rem(polygon_temp`num_vertices)(k)),
  --        running_min = distance(p, running_projection) IN
  --    projection_onto_boundary_of_recurse(p, polygon_temp, k,
  --                                        running_projection, running_min);
  --
  --  % Generalization of function below to enable use of induction scheme
  --  % backward_below_induction.
  --  projection_onto_boundary_of_gen(p: point_2d,
  --                                  polygon_temp: polygon_2d,
  --                                  k: upto(polygon_temp`num_vertices)): point_2d =
  --    LET running_projection =
  --          polygon_temp`vertices(rem(polygon_temp`num_vertices)(k)),
  --        running_min = distance(p, running_projection) IN
  --    projection_onto_boundary_of_recurse(p, polygon_temp, k,
  --                                        running_projection, running_min);
  --
  --  % Corresponds to visilibity.cpp function
  --  % Point Point::projection_onto_boundary_of(const Polygon&) const
  --  projection_onto_boundary_of(p: point_2d, polygon_temp: polygon_2d): point_2d =
  --    LET running_projection = polygon_temp`vertices(0),
  --        running_min = distance(p, running_projection) IN
  --    projection_onto_boundary_of_recurse(p, polygon_temp, 0, running_projection, running_min);
  function projection_onto_boundary_of(p: point_2d; polygon_temp: polygon_2d) return point_2d;

  -- PVS:
  --  boundary_distance_recurse(p: point_2d, polygon_temp: polygon_2d,
  --      vtx_idx: upto(polygon_temp`num_vertices), running_min: nnreal): RECURSIVE nnreal =
  --    IF vtx_idx = polygon_temp`num_vertices THEN
  --      running_min
  --    ELSE
  --      LET distance_temp = distance(p, edges_of_polygon(polygon_temp)(vtx_idx)),
  --          new_min  = IF distance_temp < running_min THEN distance_temp
  --                     ELSE running_min ENDIF IN
  --      boundary_distance_recurse(p, polygon_temp, vtx_idx+1, new_min)
  --    ENDIF
  --    MEASURE polygon_temp`num_vertices - vtx_idx;
  --
  --  % Corresponds to visilibity.cpp function
  --  % double boundary_distance(const Point&, const Polygon&)
  --  boundary_distance(p: point_2d, polygon_temp: polygon_2d): nnreal =
  --    LET running_min = distance(p, polygon_temp`vertices(0)) IN
  --    boundary_distance_recurse(p, polygon_temp, 0, running_min);
  function boundary_distance(p: point_2d; polygon_temp: polygon_2d) return nn_float;

  -- PVS:
  --  seg_to_seg_distances(A, B: polygon_2d): finite_set[nnreal] =
  --    {d: nnreal | EXISTS (ea: (edge_of_polygon?(A)), eb: (edge_of_polygon?(B))):
  --                   d = distance(ea, eb)}
  --
  --  seg_to_seg_distances_nonempty: LEMMA
  --    FORALL (A, B: polygon_2d):
  --      NOT empty?(seg_to_seg_distances(A, B))
  --
  --  % Current best guess for what boost::geometry is doing
  --  % TODO: create a SPARK Ada implementable specification such that
  --  %   distance_is_closest can be proved true
  --  distance(geometry1, geometry2: polygon_2d): nnreal =
  --    min[nnreal, <=](seg_to_seg_distances(geometry1, geometry2))
  function distance(geometry1, geometry2: polygon_2d) return nn_float;

  --  uncontained_distance(geometry1, geometry2: simple_polygon_2d): nnreal =
  --    IF polygon_contained?(geometry1, geometry2) OR
  --       polygon_contained?(geometry2, geometry1)
  --      THEN 0
  --      ELSE distance(geometry1, geometry2)
  --    ENDIF
  function uncontained_distance(geometry1, geometry2: polygon_2d) return nn_float is
    (if are_vertices_contained(geometry1, geometry2) OR
         are_vertices_contained(geometry2, geometry1) then 0.0
         else distance(geometry1, geometry2));

end perimeter_props;
