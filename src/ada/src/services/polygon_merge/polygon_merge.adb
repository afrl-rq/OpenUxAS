-- -----------------------------------------------------------------------------
-- polygon_merge.adb              Dependable Computing
-- Corresponds to logic from polygon_merge.pvs
-- -----------------------------------------------------------------------------
with segments_2d;
with between_rays;
with injection_props;

package body polygon_merge with SPARK_Mode is
  use segments_2d;
  use between_rays;
  use injection_props;

  -- From polygon_merge.pvs:
  --  next_merge_vertex(Am: simple_polygon_2d, Bm: simple_polygon_2d,
  --                    vertex: (point_AB_vtx?(Am, Bm)), prev_vtx: point_2d):
  --       (point_AB_vtx?(Am, Bm)) =
  --    LET idx_A = find_index(Am, vertex) IN
  --    LET idx_B = find_index(Bm, vertex) IN
  --      IF idx_A >= 0 THEN
  --        % current vertex is in both injected polygons
  --        IF idx_B >= 0 THEN
  --          % If I'm at the nth vertex, then my next edge is the nth edge
  --          LET e2: segment_2d = edges_of_polygon(Am)(idx_A) IN
  --          LET e3: segment_2d = edges_of_polygon(Bm)(idx_B) IN
  --        IF prev_vtx = e3`p2 OR point_between_rays?(e3, e2)(prev_vtx) THEN
  --            % e2 points more to the right than e3
  --            Am`vertices(next_index(Am, idx_A))
  --          ELSE
  --            % e3 points more rightward, or e2 and e3 overlap (and are equal)
  --            Bm`vertices(next_index(Bm, idx_B))
  --          ENDIF
  --        ELSE
  --          % vertex is only in injected polygon Am
  --          Am`vertices(next_index(Am, idx_A))
  --        ENDIF
  --      ELSE
  --        % vertex is only in injected polygon Bm
  --        % idx_B must be >= 0 due to type constraint on vertex
  --        Bm`vertices(next_index(Bm, idx_B))
  --      ENDIF
  function next_merge_vertex(Am, Bm: simple_polygon_2d; vertex: point_2d; prev_vtx: point_2d) return point_2d is
    idx_A: Integer := find_index(Am, vertex);
    idx_B: Integer := find_index(Bm, vertex);
    result: point_2d;
  begin
    if idx_A >= 0 then
      if idx_B >= 0 then
        declare
          e2: segment_2d := edges_of_polygon(Am, idx_A);
          e3: segment_2d := edges_of_polygon(Bm, idx_B);
        begin
          if prev_vtx = e3.p2 or is_point_between_rays(e3, e2, prev_vtx) then
            result := Am.vertices(next_index(Am, idx_A));
          else
            result := Bm.vertices(next_index(Bm, idx_B));
          end if;
        end;
      else
        result := Am.vertices(next_index(Am, idx_A));
      end if;
    else
      result := Bm.vertices(next_index(Bm, idx_B));
    end if;
    return result;
  end next_merge_vertex;

  -- From polygon_merge.pvs:
  --   merge_helper(Am: simple_polygon_2d, Bm: simple_polygon_2d,
  --                first_vtx, vertex: (point_AB_vtx?(Am, Bm)),
  --                prev_vtx: point_2d,
  --                vtx_num: upto(Am`num_vertices + Bm`num_vertices)):
  --       RECURSIVE {s: (point_seq_AB_vtx?(Am, Bm)) |
  --                     s`length <= Am`num_vertices + Bm`num_vertices - vtx_num} =
  --     LET next_vtx: point_2d = next_merge_vertex(Am, Bm, vertex, prev_vtx) IN
  --     % If next_vtx reaches first vertex, we're done
  --     IF next_vtx = first_vtx OR vtx_num = Am`num_vertices + Bm`num_vertices
  --       THEN empty_seq
  --       ELSE
  --         % Else recurse with new vertex
  --  singleton_seq(next_vtx) o
  --           merge_helper(Am, Bm, first_vtx, next_vtx, vertex, vtx_num + 1)
  --     ENDIF
  --     MEASURE Am`num_vertices + Bm`num_vertices - vtx_num
  --
  -- AND
  --
  --  merge_seq(A: simple_polygon_2d,
  --            B: (merge_pre_condition(A))): finseq[point_2d] =
  --    LET (Am, Bm) = inject_vertices_into_polygon(A, B) IN
  --    LET first_vtx = first_vertex_merge(A, B) IN
  --      singleton_seq(first_vtx) o
  --        merge_helper(Am, Bm, first_vtx, first_vtx, prev_0(first_vtx), 0)
  -- TODO: Needs careful peer review for PVS -> SPARK Ada retrenchment
  function merge_seq(A, B: simple_polygon_2d) return bounded_vertex_list is
    first_vtx: point_2d := first_vertex_merge(A, B);
    retval: bounded_vertex_list := singleton_seq(first_vtx);
    ppair: polygon_pair := inject_vertices_into_polygon(A, B);
    vertex: point_2d := first_vtx;
    prev_vtx: point_2d := prev_0(first_vtx);
    next_vtx: point_2d := next_merge_vertex(ppair.Am, ppair.Bm, first_vtx, prev_vtx);
    vtx_num: Integer := 0;
  begin
    while next_vtx /= first_vtx AND vtx_num /= ppair.Am.num_vertices + ppair.Bm.num_vertices loop
      retval.num_vertices := retval.num_vertices + 1;
      retval.vertices(retval.num_vertices-1) := next_vtx;
      prev_vtx := vertex;
      vertex := next_vtx;
      next_vtx := next_merge_vertex(ppair.Am, ppair.Bm, vertex, prev_vtx);
      vtx_num := vtx_num + 1;
    end loop;
    return retval;
  end merge_seq;

end polygon_merge;
