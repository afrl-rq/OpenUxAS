with vectors_2d;
with vectors_cross_2d;
with segments_2d;
with segments_2d_props;

package Segment_Lemmas with SPARK_Mode,
    Ghost is -- all content is ghost
  use vectors_2d;
  use vectors_cross_2d;
  use segments_2d;
  use segments_2d_props;

  procedure Lemma_Can_Subtract_Symmetric(s1: in segment_2d; s2: in segment_2d) with
    Global => null,
    Pre => can_subtract(s1, s2),
    Post => can_subtract(s2, s1);

  procedure Lemma_Segments_Comparable_Symmetric(s1: in segment_2d; s2: in segment_2d) with
    Global => null,
    Pre => segments_comparable(s1, s2),
    Post => segments_comparable(s2, s1);

  procedure Lemma_Can_Subtract_Overlap_Pts(p: point_2d; e, s: segment_2d)
    with
      Global => null,
      Pre => segments_comparable(e, s) and then
      can_add(e.p1, vector_from_point_to_point(e.p1, e.p2)) and then
      can_subtract(p, e) and then can_subtract(p, s) and then
    can_add(e.p1.x * e.p1.x, vector_from_point_to_point(e.p1, e.p2).x * vector_from_point_to_point(e.p1, e.p2).x) and then
    can_add(e.p1.y * e.p1.y, vector_from_point_to_point(e.p1, e.p2).y * vector_from_point_to_point(e.p1, e.p2).y) and then
    are_segments_overlapping(e, s),
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


end Segment_Lemmas;
