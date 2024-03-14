-- -----------------------------------------------------------------------------
-- topleft_vertex.ads              Dependable Computing
-- Corresponds to logic from topleft_vertex.pvs
-- -----------------------------------------------------------------------------
with vectors_2d;
with polygons_2d;
with vertex_list;

package topleft_vertex with SPARK_Mode is
  use vectors_2d;
  use polygons_2d;
  use vertex_list;

  --  % Function for finding the x value of the left-most vertex
  --  leftmost_vertex_val(p: simple_polygon_2d): real =
  --    min[real, <=]({r: real | EXISTS (i: below(p`num_vertices)):
  --                               r = p`vertices(i)`x})
  --TODO: Add explanation of translation
  function leftmost_vertex_val(p: simple_polygon_2d) return vector_float_type
    with Post =>
      (for all idx in 0 .. p.num_vertices-1 =>
         leftmost_vertex_val'Result <= p.vertices(idx).x) and
      (for some idx in 0 .. p.num_vertices-1 =>
         leftmost_vertex_val'Result = p.vertices(idx).x);

  function all_leftmost_matched(p: simple_polygon_2d; stop_idx: Natural;
                                to_match: uniq_vertex_list) return Boolean is
    (for all idx in 0 .. stop_idx =>
       (if p.vertices(idx).x = leftmost_vertex_val(p) then
            (for some jdx in 0 .. to_match.num_vertices-1 =>
                 to_match.vertices(jdx) = p.vertices(idx))))
        with Ghost,
        Pre => stop_idx <= p.num_vertices-1;

  function only_lefmost_matched(p: simple_polygon_2d;
                                to_match: uniq_vertex_list) return Boolean is
    (for all idx in 0 .. to_match.num_vertices-1 =>
       to_match.vertices(idx).x = leftmost_vertex_val(p))
      with Ghost;

  --  % Function that returns a set of vertices whose x-values equal
  --  % the leftmost vertex x-value.
  --  leftmost_vertices(p: simple_polygon_2d): finite_set[point_2d] =
  --    {v: point_2d | EXISTS (i: below(p`num_vertices)):
  --                     v`x = leftmost_vertex_val(p) AND v = p`vertices(i)}
  --TODO: Add explanation of translation
  function leftmost_vertices(p: simple_polygon_2d) return uniq_vertex_list with
    Post => all_leftmost_matched(p, p.num_vertices-1, leftmost_vertices'Result) and
    only_lefmost_matched(p, leftmost_vertices'Result);

  --  % Function for finding the y value of the topmost of the leftmost vertices.
  --  topleft_vertex_val(p: simple_polygon_2d): real =
  --    max[real, <=]({y: real | EXISTS (v: (leftmost_vertices(p))): y = v`y})
  --TODO: Add explanation of translation
  function topleft_vertex_val(p: simple_polygon_2d) return vector_float_type with
    Post =>
      (for all idx in 0 .. p.num_vertices-1 =>
         (if (p.vertices(idx).x = leftmost_vertex_val(p)) then
            topleft_vertex_val'Result >= p.vertices(idx).y)) and
      (for some idx in 0 .. p.num_vertices-1 =>
         (p.vertices(idx).x = leftmost_vertex_val(p) and
            p.vertices(idx).y = topleft_vertex_val'Result));

  --  % Index value of the topmost vertex in the set of leftmost vertices.
  --  topleft_vertex_idx(p: simple_polygon_2d):
  --    {i: below(p`num_vertices) |
  --        p`vertices(i)`x = leftmost_vertex_val(p) AND
  --        p`vertices(i)`y = topleft_vertex_val(p)}
  function topleft_vertex_idx(p: simple_polygon_2d) return Natural with
    Post =>
      (topleft_vertex_idx'Result < p.num_vertices) and then
      (p.vertices(topleft_vertex_idx'Result).x = leftmost_vertex_val(p)) and then
      (p.vertices(topleft_vertex_idx'Result).y = topleft_vertex_val(p));

end topleft_vertex;
