-- -----------------------------------------------------------------------------
-- topleft_vertex.adb              Dependable Computing
-- Corresponds to logic from topleft_vertex.pvs
-- -----------------------------------------------------------------------------
package body topleft_vertex with SPARK_Mode is

  --  % Function for finding the x value of the left-most vertex
  --  leftmost_vertex_val(p: simple_polygon_2d): real =
  --    min[real, <=]({r: real | EXISTS (i: below(p`num_vertices)):
  --                               r = p`vertices(i)`x})
  function leftmost_vertex_val(p: simple_polygon_2d) return vector_float_type is
    result: vector_float_type := p.vertices(0).x;
  begin
    for idx in 1 .. p.num_vertices-1 loop
      if p.vertices(idx).x < result then
        result := p.vertices(idx).x;
      end if;
      pragma Loop_Invariant(for some jdx in 0 .. idx =>
                              result = p.vertices(jdx).x);
      pragma Loop_Invariant(for all jdx in 0 .. idx =>
                              result <= p.vertices(jdx).x);
    end loop;
    Pragma Assert(for some idx in 0 .. p.num_vertices-1 =>
         result = p.vertices(idx).x);
    return result;
  end leftmost_vertex_val;

  --  % Function that returns a set of vertices whose x-values equal
  --  % the leftmost vertex x-value.
  --  leftmost_vertices(p: simple_polygon_2d): finite_set[point_2d] =
  --    {v: point_2d | EXISTS (i: below(p`num_vertices)):
  --                     v`x = leftmost_vertex_val(p) AND v = p`vertices(i)}
  function leftmost_vertices(p: simple_polygon_2d) return uniq_vertex_list is
    result: uniq_vertex_list := empty_seq;
    ctr: Natural := 0;
    leftmost_val: vector_float_type := leftmost_vertex_val(p);
  begin
    pragma Assert(for some idx in 0 .. p.num_vertices-1 =>
         leftmost_val = p.vertices(idx).x);
    for idx in 0 .. p.num_vertices-1 loop
      if p.vertices(idx).x = leftmost_val then
        result.vertices(ctr) := p.vertices(idx);
        ctr := ctr + 1;
        result.num_vertices := ctr;
      end if;
      pragma Loop_Invariant(ctr = result.num_vertices);
      pragma Loop_Invariant((ctr > 0) or
                              (for some jdx in (idx+1) .. p.num_vertices-1 =>
                                 leftmost_Val = p.vertices(jdx).x));
      -- Should follow directly from the above
      pragma Loop_Invariant((idx < p.num_vertices-1) or (ctr > 0));
      pragma Loop_Invariant(ctr <= idx+1);
      pragma Loop_Invariant(ctr = result.num_vertices);
      pragma Loop_Invariant((ctr = 0) or else
                              (for all jdx in 0 .. ctr-1 =>
                                 (for all kdx in idx+1 .. p.num_vertices-1 =>
                                      can_subtract(p.vertices(kdx),
                                    result.vertices(jdx)))));
      pragma Loop_Invariant((ctr = 0) or else
                              (for all jdx in 0 .. ctr-1 =>
                                 (for all kdx in idx+1 .. p.num_vertices-1 =>
                                      can_subtract(result.vertices(jdx),
                                    p.vertices(kdx)))));
      pragma Loop_Invariant((ctr = 0) or else
                              (for all jdx in 0 .. ctr-1 =>
                                 (for some kdx in 0 .. idx =>
                                      result.vertices(jdx) = p.vertices(kdx)
                                 )));
      pragma Loop_Invariant((ctr = 0) or else
                              (for all jdx in 0 .. ctr-1 =>
                                 (for all kdx in idx+1 .. p.num_vertices-1 =>
                                      result.vertices(jdx) /= p.vertices(kdx)
                                 )));
      pragma Loop_Invariant(for all jdx in 0 .. idx =>
                             (if p.vertices(jdx).x = leftmost_val then
                               (for some kdx in 0 .. result.num_vertices-1 =>
                                 result.vertices(kdx) = p.vertices(jdx))));
      -- Should follow directly from above
      pragma Loop_Invariant((ctr = 0) or else all_leftmost_matched(p, idx, result));
      pragma Loop_Invariant(only_lefmost_matched(p, result));
    end loop;
    pragma Assert(ctr > 0);
    return result;
  end leftmost_vertices;

  function topleft_vertex_val(p: simple_polygon_2d) return vector_float_type is
    result: vector_float_type := 0.0;
    any_found: Boolean := false;
    leftmost_val: vector_float_type := leftmost_vertex_val(p);
  begin
    for idx in 0 .. p.num_vertices-1 loop
      if p.vertices(idx).x = leftmost_val then
        if any_found then
          if p.vertices(idx).y > result then
            result := p.vertices(idx).y;
          end if;
        else
          any_found := true;
          result := p.vertices(idx).y;
        end if;
      end if;
      pragma Loop_Invariant(for all jdx in 0 .. idx =>
                              (if p.vertices(jdx).x = leftmost_val then
                                   any_found = true));
      pragma Loop_Invariant(for all jdx in 0 .. idx =>
                              (if p.vertices(jdx).x = leftmost_val then
                                   result >= p.vertices(jdx).y));
    end loop;
    return result;
  end topleft_vertex_val;

  --  % Index value of the topmost vertex in the set of leftmost vertices.
  --  topleft_vertex_idx(p: simple_polygon_2d):
  --    {i: below(p`num_vertices) |
  --        p`vertices(i)`x = leftmost_vertex_val(p) AND
  --        p`vertices(i)`y = topleft_vertex_val(p)}
  function topleft_vertex_idx(p: simple_polygon_2d) return Natural is
    result: Natural := p.num_vertices;
    leftmost_val: vector_float_type := leftmost_vertex_val(p);
    topleftmost_val: vector_float_type := topleft_vertex_val(p);
  begin
    for idx in 0 .. p.num_vertices-1 loop
      -- The following assume corresponds to topleft_vertex_val_TCC1
      pragma Assume(for some jdx in 0 .. p.num_vertices-1 =>
                      (p.vertices(jdx).x = leftmost_vertex_val(p) and
                           p.vertices(jdx).y = topleft_vertex_val(p)));
      if p.vertices(idx).x = leftmost_val then
        if p.vertices(idx).y = topleftmost_val then
          result := idx;
          pragma Assert(p.vertices(result).x = leftmost_vertex_val(p));
          pragma Assert(p.vertices(result).y = topleft_vertex_val(p));
          exit;
        end if;
      end if;
      -- If we've made it this far, we know that we have not yet found the topleft idx
      pragma Loop_Invariant(for all jdx in 0 .. idx =>
                              not (p.vertices(jdx).x = leftmost_vertex_val(p) and
                                    p.vertices(jdx).y = topleft_vertex_val(p)));
      -- Thus, from the assume above, we know one of the remaining vertices must be it
      pragma Loop_Invariant(for some jdx in idx+1 .. p.num_vertices-1 =>
                              (p.vertices(jdx).x = leftmost_vertex_val(p) and
                                   p.vertices(jdx).y = topleft_vertex_val(p)));
    end loop;
    pragma Assert(result < p.num_vertices);
    pragma Assert(p.vertices(result).x = leftmost_vertex_val(p));
    pragma Assert(p.vertices(result).y = topleft_vertex_val(p));
    return result;
  end topleft_vertex_idx;

end topleft_vertex;
