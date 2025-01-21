-- -----------------------------------------------------------------------------
-- perimeter_props.ads               Dependable Computing
-- Corresponds to logic from perimeter_props.pvs
-- -----------------------------------------------------------------------------
with vectors_2d;
with polygons_2d;

package horiz_edge_crossing with SPARK_Mode is
  use vectors_2d;
  use polygons_2d;

  -- Original PVS:
  --  % Here is the condition for polygon edge containment. A is contained
  --  % within C if every point on every edge of A is either inside of C or
  --  % lies on an edge of C.
  --  polygon_contained?(A, C: simple_polygon_2d): bool =
  --    FORALL (p: point_2d):
  --      point_on_polygon_perimeter?(A)(p) IMPLIES
  --        point_in_polygon?(p, C) OR point_on_polygon_perimeter?(C)(p)
  --NB: The above is not implementable, so we changed to implement are_vertices_contained
  -- which is implied by polygon_contained? but does not imply polygon_contained?
  -- because an edge of a polygon could leave and then re-enter a polygon in which
  -- the vertices are contained.
  -- Here is the PVS version of *that* function (contained in perimeter_props for now):
  --  are_vertices_contained?(A, C: simple_polygon_2d): bool =
  --    FORALL (p_idx: below(A`num_vertices)):
  --      is_point_in_polygon_exclusive?(C)(A`vertices(p_idx)) OR
  --        is_point_on_polygon_perimeter?(C)(A`vertices(p_idx));
  function are_vertices_contained(A, C: simple_polygon_2d) return Boolean is
    (for all p_idx in 0 .. A.num_vertices-1 =>
       is_point_in_polygon_exclusive(C, A.vertices(p_idx)) or
         is_point_on_polygon_perimeter(C, A.vertices(p_idx)));

end horiz_edge_crossing;
