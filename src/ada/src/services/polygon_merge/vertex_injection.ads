-- -----------------------------------------------------------------------------
-- vertex_injection.ads              Dependable Computing
-- Corresponds to logic from vertex_injection.pvs
-- -----------------------------------------------------------------------------
-- Build segments off of vectors and points.
with segments_2d;
with polygons_2d;
with segments_2d_props;
with vertex_list;
with vectors_2d;
with vectors_cross_2d;

package vertex_injection with SPARK_Mode is
  use segments_2d;
  use polygons_2d;
  use segments_2d_props;
  use vertex_list;
  use vectors_2d;
  use vectors_cross_2d;

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
  function injected_edge(e: segment_2d; B: simple_polygon_2d) return bounded_vertex_list
    with Pre => B.num_vertices <= (MAX_NUM_VERTICES / 2) - 1 and then
    (for all i in 0 .. B.num_vertices-1 =>
       segments_comparable(e, edges_of_polygon(B, i)))
    and then can_add(e.p1, vector_from_point_to_point(e.p1, e.p2));

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
  function injected_edge_seq(e: segment_2d; P: bounded_vertex_list) return bounded_vertex_list;
  --TODO: Add precondition

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
  function injected_vertices(A, B: simple_polygon_2d) return bounded_vertex_list;

end vertex_injection;
