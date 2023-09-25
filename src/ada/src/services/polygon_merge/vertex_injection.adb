-- -----------------------------------------------------------------------------
-- vertex_injection.adb              Dependable Computing
-- Corresponds to logic from vertex_injection.pvs
-- -----------------------------------------------------------------------------
with Vector_Lemmas;
with Segment_Lemmas;
with floats;
with prelude;

package body vertex_injection with SPARK_Mode is
  use Vector_Lemmas;
  use Segment_Lemmas;
  use floats;
  use prelude;

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
    -- The following is just so we can introduce bread crumbs for the prover
    ghost_array: bounded_vertex_array := (0 => e.p1, others =>zero);
    retval: bounded_vertex_list;
    s: segment_2d;
  begin
    Lemma_Can_Subtract_Degenerate(ghost_array(0));
    -- PA #1
    pragma Assert(can_subtract(ghost_array(0), ghost_array(0)));
    pragma Assert(for all i in 0 .. 0 =>
                    (for all j in 0 .. 0 =>
                       can_subtract(ghost_array(i), ghost_array(j))));

    -- predicate check might fail, unless run at very long proof times
    -- Initialize with the p = e`p1 case
    retval := (num_vertices => 1, vertices => ghost_array);

    -- PA #3
    pragma Assert(B.num_vertices <= (MAX_NUM_VERTICES / 2) - 1);
    pragma Assert(B.num_vertices * 2 + retval.num_vertices <= MAX_NUM_VERTICES);
    pragma Assert(retval.num_vertices = 1);
    pragma Assert(next_index(retval, 0) = 0);
    pragma Assert(can_subtract(e, (p1 => e.p1, p2 => e.p1)) and
                    can_subtract(weak_segment_2d'(p1 => e.p1, p2 => e.p1), e));
    -- PA #8
    pragma Assert(weak_edges_of_polygon(retval, 0) = (p1 => e.p1, p2 => e.p1));
    --segments_comparable:
    --  (can_subtract(s1, s2) and then can_subtract(s2, s1) and then
    --   segment_2d_constraint(s1.p1, s1.p2) and then segment_2d_constraint(s2.p1, s2.p2) and then
    --   can_add(s1.p1, vector_from_point_to_point(s1.p1, s1.p2)) and then
    --   can_add(s1.p1.x * s1.p1.x, vector_from_point_to_point(s1.p1, s1.p2).x * vector_from_point_to_point(s1.p1, s1.p2).x) and then
    --   can_add(s1.p1.y * s1.p1.y, vector_from_point_to_point(s1.p1, s1.p2).y * vector_from_point_to_point(s1.p1, s1.p2).y))
    -- PA #9
    pragma Assert(weak_edges_of_polygon(retval, 0).p1 = weak_edges_of_polygon(retval, 0).p2);
    declare
      p1: Vect2 := weak_edges_of_polygon(retval, 0).p1;
      p2: Vect2 := weak_edges_of_polygon(retval, 0).p2;
    begin
      -- PA #9.1
      pragma Assert(can_subtract(p2, p1) and then can_subtract(p1, p2));
      pragma Assert(vector_2d_constraint(p2.x - p1.x, p2.y - p1.y));
      pragma Assert(vector_from_point_to_point(p1, p2) = zero);
      Lemma_Can_Always_Add_Zero_Vec(p1);
      pragma Assert(can_add(p1, zero));
      pragma Assert(can_add(p1, vector_from_point_to_point(p1, p2)));
      -- PA #9.6
      pragma Assert(can_add(p2, vector_from_point_to_point(p2, p1)));
      pragma Assert(vector_from_point_to_point(p1, p2).x * vector_from_point_to_point(p1, p2).x = 0.0);
      pragma Assert(add_constraint(p1.x * p1.x, 0.0));
      pragma Assert(add_constraint(p1.x * p1.x,
                    vector_from_point_to_point(p1, p2).x * vector_from_point_to_point(p1, p2).x));
      pragma Assert(add_constraint(p2.x * p2.x,
                    vector_from_point_to_point(p2, p1).x * vector_from_point_to_point(p2, p1).x));
      pragma Assert(vector_from_point_to_point(p1, p2).y * vector_from_point_to_point(p1, p2).y = 0.0);
      -- PA #9.11
      pragma Assert(add_constraint(p1.y * p1.y, 0.0));
      pragma Assert(add_constraint(p1.y * p1.y,
                    vector_from_point_to_point(p1, p2).y * vector_from_point_to_point(p1, p2).y));
      pragma Assert(add_constraint(p2.y * p2.y,
                    vector_from_point_to_point(p2, p1).y * vector_from_point_to_point(p2, p1).y));
    end;
    -- PA #10
    pragma Assert(segment_2d_weak_constraint(weak_edges_of_polygon(retval, 0).p1, weak_edges_of_polygon(retval, 0).p2));
    pragma Assert(can_add(e.p1, vector_from_point_to_point(e.p1, e.p2)));
    pragma Assert(can_add(e.p1.x * e.p1.x, vector_from_point_to_point(e.p1, e.p2).x * vector_from_point_to_point(e.p1, e.p2).x));
    pragma Assert(can_add(e.p1.y * e.p1.y, vector_from_point_to_point(e.p1, e.p2).y * vector_from_point_to_point(e.p1, e.p2).y));
    -- TODO: The following won't work because the "edge" has norm = 0
    pragma Assert(segments_weak_comparable(e, weak_edges_of_polygon(retval, 0)));
    -- PA #15
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    segments_comparable(e, edges_of_polygon(B, j)));
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    can_subtract(e, edges_of_polygon(B, j)) and
                      can_subtract(edges_of_polygon(B, j), e));
    -- PA #17
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    can_subtract(e.p1, edges_of_polygon(B, j).p1) and
                      can_subtract(edges_of_polygon(B, j).p1, e.p1));
    -- PA #18
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    edges_of_polygon(B, j).p1 = B.vertices(j) and
                    B.vertices(j) = edges_of_polygon(B, j).p1);
    -- PA #19
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    Real_Eq(edges_of_polygon(B, j).p1, B.vertices(j)) and
                    Real_Eq(B.vertices(j), edges_of_polygon(B, j).p1));
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    can_subtract(e.p1, B.vertices(j)) and
                      can_subtract(B.vertices(j), e.p1));
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    (can_subtract(e.p1, edges_of_polygon(B, j).p1) and then
                     can_subtract(edges_of_polygon(B, j).p1, e.p1)) and then
                  (edges_of_polygon(B, j).p1 = B.vertices(j) and then
                  B.vertices(j) = edges_of_polygon(B, j).p1) and then
                    (can_subtract(e.p1, B.vertices(j)) and then
                       can_subtract(B.vertices(j), e.p1)));
    pragma Assert(for all j in 0 .. B.num_vertices-1 =>
                    can_subtract(retval.vertices(0), B.vertices(j)) and
                      can_subtract(B.vertices(j), retval.vertices(0)));
    -- PA #22
    pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                    (for all j in 0 .. B.num_vertices-1 =>
                       can_subtract(retval.vertices(i), B.vertices(j)) and
                         can_subtract(B.vertices(j), retval.vertices(i))));
    for idx in 0 .. B.num_vertices-1 loop
      -- TODO lvl 1
      pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                              (for all j in 0 .. B.num_vertices-1 =>
                                 can_subtract(retval.vertices(i), edges_of_polygon(B, j))));
      -- TODO lvl 1
      pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                              (for all j in 0 .. retval.num_vertices - 1 =>
                                 can_subtract(retval.vertices(i), retval.vertices(j))));
      -- LI #22.1
      pragma Loop_Invariant(retval.num_vertices >= 1);
      -- LI #22.2
      pragma Loop_Invariant((retval.num_vertices = 1 and
                              segments_weak_comparable(e, weak_edges_of_polygon(retval, 0))) or
                                (for all i in 0 .. retval.num_vertices-1 =>
                                   segments_comparable(e, weak_edges_of_polygon(retval, i))));
      -- LI #22.3
      pragma Loop_Invariant(for all i in 0 .. B.num_vertices-1 =>
                              segments_comparable(e, edges_of_polygon(B, i)));
      -- LI #22.4
      pragma Loop_Invariant((B.num_vertices - idx) * 2 + retval.num_vertices <= MAX_NUM_VERTICES);
      -- LI #22.5
      pragma Loop_Invariant(for all i in 0 .. retval.num_vertices-1 =>
                              (for all j in 0 .. B.num_vertices-1 =>
                                 can_subtract(retval.vertices(i), B.vertices(j)) and
                                   can_subtract(B.vertices(j), retval.vertices(i))));
      -- LI #22.6
      -- This assert really shouldn't be necessary!
      pragma Loop_Invariant(for all i in 0 .. B.num_vertices-1 =>
                              (edges_of_polygon(B, i).p1 = B.vertices(i) and
                                   edges_of_polygon(B, i).p2 = B.vertices(next_index(B, i))));
      -- LI #22.7
      pragma Loop_Invariant(for all i in 0 .. retval.num_vertices-1 =>
                              (for all j in 0 .. B.num_vertices-1 =>
                                 can_subtract(retval.vertices(i), edges_of_polygon(B, j))));
      -- LI #22.8
      -- The meat of the bounded_vertex_list invariant
      pragma Loop_Invariant(for all i in 0 .. retval.num_vertices-1 =>
                              (for all j in 0 .. retval.num_vertices - 1 =>
                                 can_subtract(retval.vertices(i), retval.vertices(j))));
      -- PA #22.1
      pragma Assert(retval.num_vertices <= MAX_NUM_VERTICES - 2);

      s := edges_of_polygon(B, idx);

      pragma Assert(segments_comparable(e, s));
      -- PA #22.3
      pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                      (for all j in 0 .. B.num_vertices-1 =>
                         can_subtract(retval.vertices(i), B.vertices(j)) and
                           can_subtract(B.vertices(j), retval.vertices(i))));
      if are_segments_overlapping(e, s) then
        -- Adding first point
        -- FAN verification point
        retval.vertices(retval.num_vertices) := min_overlap_point(e, s);

        -- Reiterate that all prior vertices can still be subtracted from each other
        -- PA #22.3.1
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. retval.num_vertices-1 =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));
        pragma Assert(for all i in 0 .. B.num_vertices-1 =>
                              segments_comparable(e, edges_of_polygon(B, i)));
        -- Assert that the new vertex can be subtracted from all of the other vertices
        -- and vice-versa
        pragma Assert(segments_comparable(e, s));
        pragma Assert(can_add(e.p1, vector_from_point_to_point(e.p1, e.p2)));
        -- PA #22.3.5
        pragma Assert(can_add(e.p1.x * e.p1.x,
                      vector_from_point_to_point(e.p1, e.p2).x * vector_from_point_to_point(e.p1, e.p2).x));
        pragma Assert(can_add(e.p1.y * e.p1.y,
                      vector_from_point_to_point(e.p1, e.p2).y * vector_from_point_to_point(e.p1, e.p2).y));
        -- PA #22.3.7
        pragma Assert(are_segments_overlapping(e, s));
        for i in 0 .. retval.num_vertices-1 loop
          -- TODO: follows from LI #3 and point_between_valid_pts_valid PVS lemma
          -- PA #22.3.7.1
          pragma Assume(can_subtract(retval.vertices(i), e));
          pragma Assert(can_subtract(retval.vertices(i), s));
          Lemma_Can_Subtract_Overlap_Pts(retval.vertices(i), e, s);
        end loop;
        -- At this point, this should still hold from LI #22.8
        -- PA #22.3.8
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. retval.num_vertices-1 =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));
        -- TODO: follows from PA #22.3.8 and point_between_valid_pts_valid PVS lemma
        pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                        can_subtract(retval.vertices(i), min_overlap_point(e, s)) and
                        can_subtract(min_overlap_point(e, s), retval.vertices(i)));
        pragma Assert(retval.vertices(retval.num_vertices) = min_overlap_point(e, s));
        -- Assert that the new vertex can be subtracted from all other vertices
        -- PA #22.3.11
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        can_subtract(retval.vertices(i), retval.vertices(retval.num_vertices)) and
                        can_subtract(retval.vertices(retval.num_vertices), retval.vertices(i)));
        -- Assert that all vertices, including the new vertex, can be subtracted from each other
        -- TODO: This follows automatically from PA #22.3.8 and 22.3.11
        pragma Assume(for all i in 0 .. retval.num_vertices =>
                        (for all j in 0 .. retval.num_vertices =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));
        pragma Assert(retval.num_vertices <= MAX_NUM_VERTICES - 2);

        -- Incrementing counter to account for first added point
        retval.num_vertices := retval.num_vertices + 1;

        -- Necessary for comparable loop invariant
        -- TODO lvl 1
        pragma Assume(segments_comparable(e,
                      weak_edges_of_polygon(retval, retval.num_vertices-2)));
        -- TODO lvl 1
        pragma Assume(segments_comparable(e,
                      weak_edges_of_polygon(retval, retval.num_vertices-1)));
        -- TODO lvl 1
        pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                        segments_comparable(e, weak_edges_of_polygon(retval, i)));
        -- Necessary for comparable loop invariant
        -- TODO: Follows from
        -- TODO lvl 1
        pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. B.num_vertices-1 =>
                           can_subtract(retval.vertices(i), B.vertices(j)) and
                             can_subtract(B.vertices(j), retval.vertices(i))));
        -- Reiterate that all prior vertices can still be subtracted from each other
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. retval.num_vertices-1 =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));

        -- Adding second point
        -- FAN reference point
        retval.vertices(retval.num_vertices) := max_overlap_point(e, s);

        -- Assert that the new vertex can be subtracted from all of the other vertices
        -- and vice-versa
        -- TODO: Follows from precondition, max_overlap_point lying on existing segment,
        -- and point_between_valid_pts_valid PVS lemma
        pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                        can_subtract(retval.vertices(i), max_overlap_point(e, s)));
        pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                        can_subtract(max_overlap_point(e, s), retval.vertices(i)));
        -- Assert that all vertices, including the new vertex, can be subtracted from each other
        -- TODO lvl 1
        pragma Assume(for all i in 0 .. retval.num_vertices =>
                        (for all j in 0 .. retval.num_vertices =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));
        pragma Assert(retval.num_vertices <= MAX_NUM_VERTICES - 1);

        -- Incrementing counter to account for second added point
        retval.num_vertices := retval.num_vertices + 1;

        -- Necessary for comparable loop invariant
        -- TODO Follows from precondition, both added points lying on existing edges,
        -- and point_between_valid_pts_valid PVS lemma
        pragma Assume(segments_comparable(e,
                      weak_edges_of_polygon(retval, retval.num_vertices-2)));
        pragma Assume(segments_comparable(e,
                      weak_edges_of_polygon(retval, retval.num_vertices-1)));
        -- TODO lvl 1
        pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                        segments_comparable(e, weak_edges_of_polygon(retval, i)));
        -- Necessary for comparable loop invariant
        -- TODO lvl 1
        pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. B.num_vertices-1 =>
                           can_subtract(retval.vertices(i), B.vertices(j)) and
                             can_subtract(B.vertices(j), retval.vertices(i))));
      elsif are_segments_intersecting(e, s) then
        -- FAN reference point
        retval.vertices(retval.num_vertices) := segment_intersect_kernel(e, s).intersect_location;

        -- Reiterate that all prior vertices can still be subtracted from each other
        pragma Assert(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. retval.num_vertices-1 =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));
        -- Assert that the new vertex can be subtracted from all of the other vertices
        -- and vice-versa
        -- TODO lvl 1
        pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                        can_subtract(retval.vertices(i),
                          segment_intersect_kernel(e, s).intersect_location));
        -- TODO lvl 1
        pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                        can_subtract(segment_intersect_kernel(e, s).intersect_location,
                          retval.vertices(i)));
        -- Assert that all vertices, including the new vertex, can be subtracted from each other
        -- TODO lvl 1
        pragma Assume(for all i in 0 .. retval.num_vertices =>
                        (for all j in 0 .. retval.num_vertices =>
                           can_subtract(retval.vertices(i),
                             retval.vertices(j))));
        pragma Assert(retval.num_vertices <= MAX_NUM_VERTICES - 2);

        retval.num_vertices := retval.num_vertices + 1;

        -- Necessary for comparable loop invariant
        -- TODO lvl 1
        pragma Assume(uniq_vertex_list_pred(retval.num_vertices, retval.vertices));
        -- TODO lvl 1
        pragma Assume(polygon_2d_constraint(retval));
        -- TODO lvl 1
        pragma Assume(segments_comparable(e,
                      edges_of_polygon(retval, retval.num_vertices-2)));
        -- TODO lvl 1
        pragma Assume(segments_comparable(e,
                      edges_of_polygon(retval, retval.num_vertices-1)));
        -- TODO lvl 1
        pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                        segments_comparable(e, weak_edges_of_polygon(retval, i)));
        -- Necessary for comparable loop invariant
        -- TODO lvl 1
        pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                        (for all j in 0 .. B.num_vertices-1 =>
                           can_subtract(retval.vertices(i), B.vertices(j)) and
                             can_subtract(B.vertices(j), retval.vertices(i))));
      end if;
      -- Corresponds to LI #5
      -- VC proves with very long proof time (> 4800s), so changing to Assume
      pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                      (for all j in 0 .. B.num_vertices-1 =>
                         can_subtract(retval.vertices(i), B.vertices(j)) and
                           can_subtract(B.vertices(j), retval.vertices(i))));
      -- Corresponds to LI #7
      -- TODO lvl 1
      pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
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
    pragma Assert(for all k in 0..P.num_vertices-1 => can_subtract(S.vertices(k), e.p1));
    for i in 0..P.num_vertices-1 loop
      pragma Loop_Invariant(for all k in 0..P.num_vertices-1 => can_subtract(S.vertices(k), e.p1));
      for j in 0..P.num_vertices-1 loop
        pragma Loop_Invariant(for all k in 0..P.num_vertices-1 => can_subtract(S.vertices(k), e.p1));
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
    -- TODO lvl 1
    pragma Assume(is_vertex(S, e.p2));
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
    pragma Assert(B.num_vertices <= MAX_NUM_VERTICES / 2 - 1);
    for index in 0..A.num_vertices-1 loop
      -- TODO lvl 1
      pragma Assume(uniq_vertex_list_pred(retval.num_vertices, retval.vertices));
      pragma Assume(for all i in 0 .. retval.num_vertices-1 =>
                      (for all j in index .. A.num_vertices-1 =>
                         retval.vertices(i) /= A.vertices(j)));
      pragma Assume(for all i in 0 .. index-1 =>
                      (is_vertex(retval, A.vertices(i))));
      pragma Loop_Invariant(uniq_vertex_list_pred(retval.num_vertices, retval.vertices));
      pragma Loop_Invariant(for all i in 0 .. retval.num_vertices-1 =>
                              (for all j in index .. A.num_vertices-1 =>
                                 retval.vertices(i) /= A.vertices(j)));
      pragma Loop_Invariant(for all i in 0 .. index-1 =>
                              (is_vertex(retval, A.vertices(i))));
      pragma Loop_Invariant(B.num_vertices <= MAX_NUM_VERTICES / 2 - 1);
      -- TODO lvl 1
      pragma Assume((for all i in 0 .. B.num_vertices-1 =>
                       segments_comparable(edges_of_polygon(A, index), edges_of_polygon(B, i)))
                    and then can_add(edges_of_polygon(A, index).p1, vector_from_point_to_point(edges_of_polygon(A, index).p1, edges_of_polygon(A, index).p2)));
      declare
        e: segment_2d := edges_of_polygon(A, index);
        P: bounded_vertex_list := injected_edge(e, B);
      begin
        -- TODO lvl 1
        pragma Assume(for all k in 0..P.num_vertices-1 => can_subtract(P.vertices(k), e.p1));
        declare
          seq: bounded_vertex_list := injected_edge_seq(e, injected_edge(e, B));
        begin
          for j in 0 .. seq.num_vertices-1 loop
            -- TODO lvl 1
            pragma Assume(for all i in 0 .. index-1 =>
                            (is_vertex(retval, A.vertices(i))));
            pragma Loop_Invariant(uniq_vertex_list_pred(retval.num_vertices, retval.vertices));
            pragma Loop_Invariant(for all i in 0 .. retval.num_vertices-1 =>
                                    (for all k in (index+1) .. A.num_vertices-1 =>
                                         retval.vertices(i) /= A.vertices(k)));
            pragma Loop_Invariant(for all i in 0 .. index-1 =>
                                    (is_vertex(retval, A.vertices(i))));
            -- TODO lvl 1
            pragma Assume(retval.num_vertices + j < MAX_NUM_VERTICES);
            retval.vertices(retval.num_vertices + j) := seq.vertices(j);
          end loop;
          -- TODO lvl 1
          pragma Assume(retval.num_vertices + seq.num_vertices <= MAX_NUM_VERTICES);
          pragma Assume((for all i in 0 .. retval.num_vertices + seq.num_vertices-1 =>
                           (for all j in 0 .. retval.num_vertices + seq.num_vertices - 1 =>
                              can_subtract(retval.vertices(i),
                                retval.vertices(j)))));
          retval.num_vertices := retval.num_vertices + seq.num_vertices;
        end;
      end;
    end loop;
    -- VC proves with very long proof time (> 4800s), so changing to Assume
    pragma Assume(uniq_vertex_list_pred(retval.num_vertices, retval.vertices));
    -- TODO lvl 1:
    pragma Assume(for all i in 0 .. A.num_vertices-1 =>
                    (is_vertex(retval, A.vertices(i))));
    return retval;
  end injected_vertices;

end vertex_injection;
