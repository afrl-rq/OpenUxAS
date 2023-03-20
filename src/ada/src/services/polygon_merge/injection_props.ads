-- -----------------------------------------------------------------------------
-- injection_props.ads              Dependable Computing
-- Corresponds to logic from injection_props.pvs
-- -----------------------------------------------------------------------------
-- Build segments off of vectors and points.
with polygons_2d;
with vertex_list;
with vertex_injection;

package injection_props with SPARK_Mode is
  use polygons_2d;
  use vertex_list;
  use vertex_injection;

  -----------------------------------------------------------------------------
  -- begin retrenchment
  -----------------------------------------------------------------------------
  type polygon_pair is record
    Am: simple_polygon_2d;
    Bm: simple_polygon_2d;
  end record;

  function bounded_vertex_list_to_polygon(bvlist: bounded_vertex_list) return simple_polygon_2d is
    (simple_polygon_2d'(num_vertices => bvlist.num_vertices,
                        vertices => bvlist.vertices));
  -----------------------------------------------------------------------------
  -- end retrenchment
  -----------------------------------------------------------------------------

  -- From injection_props.pvs
  --  inject_post_condition?(A, B: simple_polygon_2d)(C: simple_polygon_2d): bool =
  --    C = (# num_vertices := injected_vertices(A, B, A`num_vertices)`length,
  --           vertices := injected_vertices(A, B, A`num_vertices)`seq #)
  function inject_post_condition(A, B: simple_polygon_2d) return simple_polygon_2d is
    -- A.num_vertices is passed in as counter for recursive function but the
    -- recursive function is converted to a loop and no longer needs this value
    -- passed in
    (bounded_vertex_list_to_polygon(injected_vertices(A, B)));

  -- From injection_props.pvs:
  --  inject_vertices_into_polygon(A: simple_polygon_2d,
  --                               B: (merge_pre_condition(A))):
  --   [(inject_post_condition?(A, B)), (inject_post_condition?(B, A))];
  function inject_vertices_into_polygon(A, B: simple_polygon_2d) return polygon_pair is
    (polygon_pair'(Am => inject_post_condition(A, B),
                   Bm => inject_post_condition(B, A)));
  --TODO: Add precondition

end injection_props;
