-- -----------------------------------------------------------------------------
-- vertex_injection.adb              Dependable Computing
-- Corresponds to logic from vertex_injection.pvs
-- -----------------------------------------------------------------------------
with Lemmas;

package body vertex_injection with SPARK_Mode is
  use Lemmas;

  --From vertex_injection.pvs:
  --  injected_edge(e: segment_2d, B: simple_polygon_2d):
  --               {Q: finite_set[point_2d] |
  --                   FORALL (p: (Q)): is_point_on_segment?(e)(p)} =
  --    {p: point_2d |
  --     p /= e`p2 AND
  --       (p = e`p1 OR
  --        EXISTS (s: (edge_of_polygon?(B))):
  --          IF are_segments_overlapping?(e)(s)
  --             THEN % p is an endpoint of the overlapping subsegment of e and s
  --                  p = min_overlap_point(e, s) OR p = max_overlap_point(e, s)
  --             ELSE % p is the intersection of e and s if it lies on both
  --                  point_on_segment?(p, e) AND point_on_segment?(p, s)
  --          ENDIF)}
  function injected_edge(e: segment_2d; B: simple_polygon_2d) return bounded_vertex_list is
    -- Initialize with the p = e`p1 case
    retval: bounded_vertex_list := (num_vertices => 1,
                                    vertices => (0 => e.p1, others => zero));
    s: segment_2d;
  begin
    pragma Assert(B.num_vertices <= (MAX_NUM_VERTICES / 2) - 1);
    pragma Assert(B.num_vertices * 2 + retval.num_vertices <= MAX_NUM_VERTICES);
    pragma Assert(retval.num_vertices = 1);
    pragma Assert(next_index(retval, 0) = 0);
    pragma Assert(can_subtract(e, (p1 => e.p1, p2 => e.p1)) and
                    can_subtract((p1 => e.p1, p2 => e.p1), e));
    pragma Assert(weak_edges_of_polygon(retval, 0) = (p1 => e.p1, p2 => e.p1));
    pragma Assert(segments_comparable(e, weak_edges_of_polygon(retval, 0)));
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    segments_comparable(e, edges_of_polygon(B, j)));
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    can_subtract(e, edges_of_polygon(B, j)) and
                      can_subtract(edges_of_polygon(B, j), e));
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    can_subtract(e.p1, edges_of_polygon(B, j).p1) and
                      can_subtract(edges_of_polygon(B, j).p1, e.p1));
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    edges_of_polygon(B, j).p1 = B.vertices(j));
    for i in 0 .. B.num_vertices-1 loop
      pragma Loop_Invariant(for all j in 0 .. i =>
                              can_subtract(e.p1, edges_of_polygon(B, j).p1) and
                                can_subtract(edges_of_polygon(B, j).p1, e.p1));
      pragma Loop_Invariant(for all j in 0 .. i =>
                              edges_of_polygon(B, j).p1 = B.vertices(j));
      pragma Loop_Invariant(for all j in 0 .. i =>
                              can_subtract(e.p1, B.vertices(j)) and
                                can_subtract(B.vertices(j), e.p1));
    end loop;
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    can_subtract(e.p1, B.vertices(j)) and
                      can_subtract(B.vertices(j), e.p1));
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    can_subtract(retval.vertices(0), B.vertices(j)) and
                      can_subtract(B.vertices(j), retval.vertices(0)));
    pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                    (for all j in 0 .. B.num_vertices-1 =>
                       can_subtract(retval.vertices(i), B.vertices(j)) and
                         can_subtract(B.vertices(j), retval.vertices(i))));
    for idx in 0 .. B.num_vertices-1 loop
      -- LI #1
      pragma Loop_Invariant(retval.num_vertices >= 1);
      -- LI #2
      pragma Loop_Invariant(for all i in 0 .. retval.num_vertices-1 =>
                              segments_comparable(e, weak_edges_of_polygon(retval, i)));
      -- LI #3
      pragma Loop_Invariant(for all i in 0 .. B.num_vertices-1 =>
                              segments_comparable(e, edges_of_polygon(B, i)));
      -- LI #4
      pragma Loop_Invariant((B.num_vertices - idx) * 2 + retval.num_vertices <= MAX_NUM_VERTICES);
      -- LI #5
      pragma Loop_Invariant(for all i in 0 .. retval.num_vertices-1 =>
                              (for all j in 0 .. B.num_vertices-1 =>
                                 can_subtract(retval.vertices(i), B.vertices(j)) and
                                   can_subtract(B.vertices(j), retval.vertices(i))));
      -- LI #6
      -- This assert really shouldn't be necessary!
      pragma Loop_Invariant(for all i in 0 .. B.num_vertices-1 =>
                              (edges_of_polygon(B, i).p1 = B.vertices(i) and
                                   edges_of_polygon(B, i).p2 = B.vertices(next_index(B, i))));
      -- LI #7
      pragma Loop_Invariant(for all i in 0 .. retval.num_vertices-1 =>
                              (for all j in 0 .. B.num_vertices-1 =>
                                 can_subtract(retval.vertices(i), edges_of_polygon(B, j).p1) and
                                   can_subtract(retval.vertices(i), edges_of_polygon(B, j).p2) and
                                 can_subtract(edges_of_polygon(B, j).p1, retval.vertices(i)) and
                                   can_subtract(edges_of_polygon(B, j).p2, retval.vertices(i))));
      -- LI #8
      -- The meat of the bounded_vertex_list invariant
      pragma Loop_Invariant(for all i in 0 .. retval.num_vertices-1 =>
                              (for all j in 0 .. retval.num_vertices - 1 =>
                                 can_subtract(retval.vertices(i),
                                   retval.vertices(j))));
      pragma Assert(retval.num_vertices <= MAX_NUM_VERTICES - 2);
      s := edges_of_polygon(B, idx);
      pragma Assert(segments_comparable(e, s));
      pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                      (for all j in 0 .. B.num_vertices-1 =>
                         can_subtract(retval.vertices(i), B.vertices(j)) and
                           can_subtract(B.vertices(j), retval.vertices(i))));
      if are_segments_overlapping(e, s) then
        retval.vertices(retval.num_vertices) := min_overlap_point(e, s);
        -- Reiterate that all prior vertices can still be subtracted from each other
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. retval.num_vertices-1 =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));
        pragma Assert(for all i in 0 .. B.num_vertices-1 =>
                              segments_comparable(e, edges_of_polygon(B, i)));
        -- Assert that the new vertex can be subtracted from all of the other vertices
        -- and vice-versa
        for i in 0 .. retval.num_vertices-1 loop
          pragma Assert(can_subtract(retval.vertices(i), s.p1));
          pragma Assert(can_subtract(retval.vertices(i), s.p2));
          pragma Assert(can_subtract(s.p1, retval.vertices(i)));
          pragma Assert(can_subtract(s.p2, retval.vertices(i)));
          Lemma_Can_Subtract_Overlap_Pts(retval.vertices(i), e, s);
        end loop;
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        can_subtract(retval.vertices(i), min_overlap_point(e, s)));
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        can_subtract(min_overlap_point(e, s), retval.vertices(i)));
        -- Assert that all vertices, including the new vertex, can be subtracted from each other
        pragma Assert(for all i in 0 .. retval.num_vertices =>
                        (for all j in 0 .. retval.num_vertices =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));
        pragma Assert(retval.num_vertices <= MAX_NUM_VERTICES - 2);
        retval.num_vertices := retval.num_vertices + 1;
        -- Necessary for comparable loop invariant
        pragma Assert(segments_comparable(e,
                      weak_edges_of_polygon(retval, retval.num_vertices-2)));
        pragma Assert(segments_comparable(e,
                      weak_edges_of_polygon(retval, retval.num_vertices-1)));
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        segments_comparable(e, weak_edges_of_polygon(retval, i)));
        -- Necessary for comparable loop invariant
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. B.num_vertices-1 =>
                           can_subtract(retval.vertices(i), B.vertices(j)) and
                             can_subtract(B.vertices(j), retval.vertices(i))));
        -- Reiterate that all prior vertices can still be subtracted from each other
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. retval.num_vertices-1 =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));
        retval.vertices(retval.num_vertices) := max_overlap_point(e, s);
        -- Assert that the new vertex can be subtracted from all of the other vertices
        -- and vice-versa
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        can_subtract(retval.vertices(i), max_overlap_point(e, s)));
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        can_subtract(max_overlap_point(e, s), retval.vertices(i)));
        -- Assert that all vertices, including the new vertex, can be subtracted from each other
        pragma Assert(for all i in 0 .. retval.num_vertices =>
                        (for all j in 0 .. retval.num_vertices =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));
        pragma Assert(retval.num_vertices <= MAX_NUM_VERTICES - 1);
        retval.num_vertices := retval.num_vertices + 1;
        -- Necessary for comparable loop invariant
        pragma Assert(segments_comparable(e,
                      weak_edges_of_polygon(retval, retval.num_vertices-2)));
        pragma Assert(segments_comparable(e,
                      weak_edges_of_polygon(retval, retval.num_vertices-1)));
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        segments_comparable(e, weak_edges_of_polygon(retval, i)));
        -- Necessary for comparable loop invariant
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. B.num_vertices-1 =>
                           can_subtract(retval.vertices(i), B.vertices(j)) and
                             can_subtract(B.vertices(j), retval.vertices(i))));
      elsif are_segments_intersecting(e, s) then
        retval.vertices(retval.num_vertices) := segment_intersect_kernel(e, s).intersect_location;
        -- Reiterate that all prior vertices can still be subtracted from each other
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. retval.num_vertices-1 =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));
        -- Assert that the new vertex can be subtracted from all of the other vertices
        -- and vice-versa
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        can_subtract(retval.vertices(i),
                          segment_intersect_kernel(e, s).intersect_location));
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        can_subtract(segment_intersect_kernel(e, s).intersect_location,
                          retval.vertices(i)));
        -- Assert that all vertices, including the new vertex, can be subtracted from each other
        pragma Assert(for all i in 0 .. retval.num_vertices =>
                        (for all j in 0 .. retval.num_vertices =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));
        pragma Assert(retval.num_vertices <= MAX_NUM_VERTICES - 2);
        retval.num_vertices := retval.num_vertices + 1;
        -- Necessary for comparable loop invariant
        pragma Assert(segments_comparable(e,
                      edges_of_polygon(retval, retval.num_vertices-2)));
        pragma Assert(segments_comparable(e,
                      edges_of_polygon(retval, retval.num_vertices-1)));
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        segments_comparable(e, weak_edges_of_polygon(retval, i)));
        -- Necessary for comparable loop invariant
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. B.num_vertices-1 =>
                           can_subtract(retval.vertices(i), B.vertices(j)) and
                             can_subtract(B.vertices(j), retval.vertices(i))));
      end if;
      -- Corresponds to LI #5
      pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                      (for all j in 0 .. B.num_vertices-1 =>
                         can_subtract(retval.vertices(i), B.vertices(j)) and
                           can_subtract(B.vertices(j), retval.vertices(i))));
      -- Corresponds to LI #7
      pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                      (for all j in 0 .. B.num_vertices-1 =>
                         can_subtract(retval.vertices(i), edges_of_polygon(B, j).p1) and
                           can_subtract(retval.vertices(i), edges_of_polygon(B, j).p2) and
                         can_subtract(edges_of_polygon(B, j).p1, retval.vertices(i)) and
                           can_subtract(edges_of_polygon(B, j).p2, retval.vertices(i))));
    end loop;
    return retval;
  end injected_edge;

  --From vertex_injection.pvs:
  --  injected_edge_seq(e: segment_2d,
  --                    P: {Q: finite_set[point_2d] |
  --                           FORALL (u: (Q)): is_point_on_segment?(e)(u)}):
  --    {S: finseq[point_2d] |
  --     S`length = card(P) AND
  --     (FORALL (v: (P)): EXISTS (i: below(S`length)): v = S`seq(i)) AND
  --     (FORALL (i: below(S`length)): member(S`seq(i), P)) AND
  --     (FORALL (i: below(S`length), j: below(i)):
  --        norm(S`seq(j) - e`p1) < norm(S`seq(i) - e`p1))}
  function injected_edge_seq(e: segment_2d; P: bounded_vertex_list) return bounded_vertex_list is
    S: bounded_vertex_list := P;
  begin
    --NB: TODO: Needs careful peer review of equality to specification
    for i in 0..P.num_vertices loop
      for j in 0..P.num_vertices loop
        if ((i /= j) and then norm(S.vertices(j) - e.p1) < norm(S.vertices(i) - e.p1)) then
          -- Swap S.seq(i) and S.seq(j)
          declare
            tmp: point_2d := S.vertices(i);
          begin
            S.vertices(i) := S.vertices(j);
            S.vertices(j) := tmp;
          end;
        end if;
      end loop;
    end loop;
    S.num_vertices := P.num_vertices;
    return S;
  end injected_edge_seq;

  --From vertex_injection.pvs:
  --  injected_vertices(A, B: simple_polygon_2d,
  --                    index: upto(A`num_vertices)): RECURSIVE finseq[point_2d] =
  --    IF index = 0
  --       THEN empty_seq
  --       ELSE LET e = edges_of_polygon(A)(index - 1) IN
  --              injected_vertices(A, B, index - 1) o
  --                injected_edge_seq(e, injected_edge(e, B))
  --    ENDIF
  --    MEASURE index
  --NB:
  -- index is passed in as counter (initialized to A.num_vertices) for the recursive
  -- function but the recursive function is converted to a loop and no longer needs
  -- this value passed in
  function injected_vertices(A, B: simple_polygon_2d) return bounded_vertex_list is
    retval: bounded_vertex_list := empty_seq;
  begin
    for index in 0..A.num_vertices-1 loop
      declare
        e: segment_2d := edges_of_polygon(A, index);
        seq: bounded_vertex_list := injected_edge_seq(e, injected_edge(e, B));
      begin
        for j in 0 .. seq.num_vertices-1 loop
          retval.vertices(retval.num_vertices + j) := seq.vertices(j);
        end loop;
        retval.num_vertices := retval.num_vertices + seq.num_vertices;
      end;
    end loop;
    return retval;
  end injected_vertices;

end vertex_injection;
