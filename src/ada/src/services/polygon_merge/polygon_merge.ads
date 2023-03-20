-- -----------------------------------------------------------------------------
-- polygon_merge.ads              Dependable Computing
-- Corresponds to logic from polygon_merge.pvs
-- -----------------------------------------------------------------------------
-- Build segments off of vectors and points.
with vectors_2d;
with polygons_2d;
with vertex_list;
with topleft_vertex;

-- Declaration of types and functions on segments.
package polygon_merge with SPARK_Mode is
  use vectors_2d;
  use polygons_2d;
  use vertex_list;
  use topleft_vertex;

  -- From polygon_merge.pvs:
  --  point_AB_vtx?(Am, Bm: simple_polygon_2d)(p: point_2d): bool =
  --      vertex?(Am)(p) OR vertex?(Bm)(p)
  function is_point_AB_vtx(Am, Bm: simple_polygon_2d; p: point_2d) return Boolean
  is
    (is_vertex(Am, p) or is_vertex(Bm, p));

  -- From polygon_merge.pvs:
  --  point_seq_AB_vtx?(Am, Bm: simple_polygon_2d)(s: finseq[point_2d]): bool =
  --    FORALL (i: below(s`length)): point_AB_vtx?(Am, Bm)(s`seq(i))
  function is_point_seq_AB_vtx(Am, Bm: simple_polygon_2d; s: bounded_vertex_list) return Boolean
  is
    (for all i in 0 .. s.num_vertices-1 => is_point_AB_vtx(Am, Bm, s.vertices(i)));

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
  function next_merge_vertex(Am, Bm: simple_polygon_2d; vertex, prev_vtx: point_2d) return point_2d
    with Pre => (is_point_AB_vtx(Am, Bm, vertex)),
      Post => (is_point_AB_vtx(Am, Bm, next_merge_vertex'Result));

  -- From polygon_merge.pvs:
  --  first_vertex_merge(A, B: simple_polygon_2d): point_2d =
  --      LET start_with_A: bool =
  --        IF leftmost_vertex_val(A) = leftmost_vertex_val(B) THEN
  --           topleft_vertex_val(A) >= topleft_vertex_val(B)
  --        ELSE
  --           leftmost_vertex_val(A) < leftmost_vertex_val(B)
  --        ENDIF IN
  --      IF start_with_A THEN
  --         A`vertices(topleft_vertex_idx(A))
  --      ELSE
  --         B`vertices(topleft_vertex_idx(B))
  --      ENDIF
  -- From supporting lemmas.pvs file:
  --  let_start_with_A_alt: LEMMA
  --  FORALL(int_1, int_2: integer, real_1, real_2: real):
  --    (IF int_1 = int_2 THEN
  --       real_1 >= real_2
  --     ELSE
  --       int_1 < int_2
  --     ENDIF) =
  --    ((int_1 = int_2 AND real_1 >= real_2) OR int_1 < int_2);
  function first_vertex_merge(A, B: simple_polygon_2d) return point_2d is
    (if ((leftmost_vertex_val(A) = leftmost_vertex_val(B) and
         topleft_vertex_val(A) >= topleft_vertex_val(B)) or
         leftmost_vertex_val(A) < leftmost_vertex_val(B)) then
       A.vertices(topleft_vertex_idx(A))
     else
       B.vertices(topleft_vertex_idx(B)));

  --  prev_0(topleft: point_2d): point_2d =
  --    % Choose an arbitrary point to the left of the topleft vertex.
  --    topleft + (# x := -1, y := 0 #)
  function prev_0(topleft: point_2d) return point_2d is
    (topleft + (x => -1.0, y => 0.0));

  -- Fn TODO:
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
  function merge_seq(A, B: simple_polygon_2d) return bounded_vertex_list;
  -- with Pre => merge_pre_condition(A, B); TODO

end polygon_merge;
