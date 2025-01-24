-- -----------------------------------------------------------------------------
-- perimeter_props.adb               Dependable Computing
-- Corresponds to logic from perimeter_props.pvs
-- -----------------------------------------------------------------------------

with segments_2d;

package body perimeter_props with SPARK_Mode is
  use segments_2d;

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
  function area(vertices: bounded_vertex_list) return Float is
    retval: Float := 0.0;
  begin
    for i in 0 .. vertices.num_vertices-1 loop
      retval :=
        vertices.vertices(i).x * modget(vertices, i+1).y -
        modget(vertices, i+1).x * vertices.vertices(i).y +
        retval;
    end loop;
    return retval;
  end area;

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
  function projection_onto_boundary_of(p: point_2d; polygon_temp: polygon_2d) return point_2d is
    running_projection: point_2d := polygon_temp.vertices(0);
    running_min: Float := distance(p, running_projection);
  begin
    for vtx_idx in 0 .. polygon_temp.num_vertices-1 loop
      declare
        pointemp: point_2d := projection_onto(p, edges_of_polygon(polygon_temp, vtx_idx));
        new_dist: Float := distance(p, pointemp);
      begin
        if new_dist < running_min then
          running_projection := pointemp;
          running_min := new_dist;
        end if;
      end;
    end loop;
    return running_projection;
  end projection_onto_boundary_of;

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
  function boundary_distance(p: point_2d; polygon_temp: polygon_2d) return nn_float is
    running_min : nn_float := distance(p, polygon_temp.vertices(0));
  begin
    for vtx_idx in 0 .. polygon_temp.num_vertices-1 loop
      declare
        distance_temp: nn_float := distance(p, edges_of_polygon(polygon_temp, vtx_idx));
      begin
        if distance_temp < running_min then
          running_min := distance_temp;
        end if;
      end;
    end loop;
    return running_min;
  end boundary_distance;

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
  function distance(geometry1, geometry2: polygon_2d) return nn_float is
    running_min: nn_float := nn_float'Last;
  begin
    for geom1_vtx_idx in 0 .. geometry1.num_vertices-1 loop
      for geom2_vtx_idx in 0 .. geometry2.num_vertices-1 loop
        declare
          ea: segment_2d := edges_of_polygon(geometry1, geom1_vtx_idx);
          eb: segment_2d := edges_of_polygon(geometry2, geom2_vtx_idx);
          new_dist: nn_float := distance(ea, eb);
        begin
          if new_dist < running_min then
            running_min := new_dist;
          end if;
        end;
      end loop;
    end loop;
    return running_min;
  end;

end perimeter_props;
