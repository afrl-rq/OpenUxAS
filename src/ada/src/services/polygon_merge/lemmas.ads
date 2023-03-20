with vectors_2d;
with segments_2d;
with segments_2d_props;
with polygons_2d;

package Lemmas with SPARK_Mode,
    Ghost is -- all content is ghost
  use vectors_2d;
  use segments_2d;
  use segments_2d_props;
  use polygons_2d;

  procedure Lemma_NonNeg_Mult_By_O_to_1_LE_Orig(x: Float; t: Float) with
    Global => null,
    Pre => x >= 0.0 and 0.0 <= t and t <= 1.0,
    Post => t * x <= x;

  procedure Lemma_Neg_Mult_By_O_to_1_GE_Orig(x: Float; t: Float) with
    Global => null,
    Pre => x < 0.0 and 0.0 <= t and t <= 1.0,
    Post => x <= t * x;

  procedure Lemma_Vector_2d_Constraint_Even(x: vector_float_type; y: vector_float_type) with
    Global => null,
    Pre => vector_2d_constraint(x, y),
    Post => vector_2d_constraint(-x, -y);

  procedure Lemma_Can_Add_Symmetric(a: vector_float_type; b: vector_float_type) with
    Global => null,
    Pre => can_add(a, b),
    Post => can_add(b, a);

  procedure Lemma_Can_Add_Even(a: vector_float_type; b: vector_float_type) with
    Global => null,
    Pre => can_add(a, b),
    Post => can_add(-a, -b);

  procedure Lemma_Can_Subtract_Symmetric(p1: vector_2d; p2: vector_2d) with
    Global => null,
    Pre => can_subtract(p1, p2),
    Post => can_subtract(p2, p1);

  procedure Lemma_Segments_Comparable_Symmetric(s1: in segment_2d; s2: in segment_2d) with
    Global => null,
    Pre => segments_comparable(s1, s2),
    Post => segments_comparable(s2, s1);

  procedure Lemma_Pts_Can_Subtract_Implies_Segment_Endpts_Can_Subtract(G: polygon_2d; p: point_2d)
    with
      Global => null,
      Pre => (for all i in 0 .. G.num_vertices-1 =>
                can_subtract(G.vertices(i), p) and can_subtract(p, G.vertices(i))),
      Post => (for all i in 0 .. G.num_vertices-1 =>
                 can_subtract(edges_of_polygon(G, i).p1, p) and
                   can_subtract(edges_of_polygon(G, i).p2, p) and
                 can_subtract(p, edges_of_polygon(G, i).p1) and
                   can_subtract(p, edges_of_polygon(G, i).p2) and
                 (for all j in 0 .. G.num_vertices-1 =>
                        can_subtract(edges_of_polygon(G, i).p1, edges_of_polygon(G, j).p1) and
                    can_subtract(edges_of_polygon(G, i).p1, edges_of_polygon(G, j).p2) and
                      can_subtract(edges_of_polygon(G, i).p2, edges_of_polygon(G, j).p1) and
                    can_subtract(edges_of_polygon(G, i).p2, edges_of_polygon(G, j).p2)));

  procedure Lemma_Can_Subtract_Overlap_Pts(p: point_2d; e, s: segment_2d)
    with
      Global => null,
      Pre => are_segments_overlapping(e, s) and then
      can_subtract(p, s.p1) and then can_subtract(p, s.p2) and then
    can_subtract(s.p1, p) and then can_subtract(s.p2, p),
    Post => can_subtract(p, min_overlap_point(e, s)) and
    can_subtract(min_overlap_point(e, s), p) and
    can_subtract(p, max_overlap_point(e, s)) and
    can_subtract(max_overlap_point(e, s), p);

  --  procedure Lemma_Segments_Intersect_Implies_Intersection_Can_Subtract(s1: in segment_2d; s2: in segment_2d) with
  --    Global => null,
  --    Pre => segments_comparable(s1, s2) and then
  --            segment_intersect_kernel(s1, s2).flag = Intersecting,
  --    Post => can_subtract(segment_intersect_kernel(s1, s2).point, s1.p1) and
  --            can_subtract(segment_intersect_kernel(s1, s2).point, s2.p1);

  --  procedure Lemma_Transitive_Vertex_Inclusion(p1 : polygon_2d; p2 : polygon_2d;
  --                                              n_p1 : polygon_2d;
  --                                              n_p2 : polygon_2d;
  --                                              vertices : uniq_vertex_list;
  --                                              merged : polygon_2d)
  --    with
  --      Global => null,
  --      Pre =>
  --        ((for all v of n_p1.vertices =>
  --                      in_array(v, p1.vertices) or in_array(v, vertices))
  --          and
  --            (for all v of n_p2.vertices =>
  --                          in_array(v,p2.vertices) or in_array(v,vertices))
  --          and
  --            (for all v of merged.vertices =>
  --                          in_array(v,n_p1.vertices) or in_array(v,n_p2.vertices))),
  --      Post =>
  --        (for all v of merged.vertices =>
  --          in_array(v,p1.vertices) or
  --            in_array(v,p2.vertices) or
  --            in_array(v,vertices));

end Lemmas;
