--with prelude;
with vector_lemmas;

package body Segment_Lemmas with SPARK_mode is

  --use prelude;
  use vector_lemmas;

--  pragma Warnings
--    (Off, "postcondition does not check the outcome of calling");

  procedure Lemma_Can_Subtract_Symmetric(s1: in segment_2d; s2: in segment_2d)
  is begin
    -- (can_subtract(s1.p1, s2.p1) and can_subtract(s1.p1, s2.p2) and
    --  can_subtract(s1.p2, s2.p1) and can_subtract(s1.p2, s2.p2));
    Lemma_Can_Subtract_Symmetric(s1.p1, s2.p1);
    pragma Assert(can_subtract(s2.p1, s1.p1));
    Lemma_Can_Subtract_Symmetric(s1.p2, s2.p1);
    pragma Assert(can_subtract(s2.p1, s1.p2));
    Lemma_Can_Subtract_Symmetric(s1.p1, s2.p2);
    pragma Assert(can_subtract(s2.p2, s1.p1));
    Lemma_Can_Subtract_Symmetric(s1.p2, s2.p2);
    pragma Assert(can_subtract(s2.p2, s1.p2));
  end Lemma_Can_Subtract_Symmetric;

  procedure Lemma_Segments_Comparable_Symmetric(s1: in segment_2d; s2: in segment_2d)
  is begin
    -- (can_subtract(s1.p1, s2.p1) and can_subtract(s1.p1, s2.p2) and
    --  can_subtract(s1.p2, s2.p1) and can_subtract(s1.p2, s2.p2));
    Lemma_Can_Subtract_Symmetric(s1.p1, s2.p1);
    pragma Assert(can_subtract(s2.p1, s1.p1));
    Lemma_Can_Subtract_Symmetric(s1.p2, s2.p1);
    pragma Assert(can_subtract(s2.p1, s1.p2));
    Lemma_Can_Subtract_Symmetric(s1.p1, s2.p2);
    pragma Assert(can_subtract(s2.p2, s1.p1));
    Lemma_Can_Subtract_Symmetric(s1.p2, s2.p2);
    pragma Assert(can_subtract(s2.p2, s1.p2));
    pragma Assert(can_subtract(s2, s1));
    pragma Assert(can_subtract(s1, s2));
    pragma Assert(segment_2d_constraint(s2.p1, s2.p2));
    pragma Assert(segment_2d_constraint(s1.p1, s1.p2));
    pragma Assert(can_add(s2.p1, vector_from_point_to_point(s2.p1, s2.p2)));
    pragma Assert(can_add(s2.p1.x * s2.p1.x,
                  vector_from_point_to_point(s2.p1, s2.p2).x * vector_from_point_to_point(s2.p1, s2.p2).x));
    pragma Assert(can_add(s2.p1.y * s2.p1.y,
                  vector_from_point_to_point(s2.p1, s2.p2).y * vector_from_point_to_point(s2.p1, s2.p2).y));
    pragma Assert(can_subtract(s2, s1) and then
                  can_subtract(s1, s2) and then
                  segment_2d_constraint(s2.p1, s2.p2) and then
                  segment_2d_constraint(s1.p1, s1.p2) and then
                  can_add(s2.p1, vector_from_point_to_point(s2.p1, s2.p2)) and then
                  can_add(s2.p1.x * s2.p1.x, vector_from_point_to_point(s2.p1, s2.p2).x * vector_from_point_to_point(s2.p1, s2.p2).x) and then
                  can_add(s2.p1.y * s2.p1.y, vector_from_point_to_point(s2.p1, s2.p2).y * vector_from_point_to_point(s2.p1, s2.p2).y));
    pragma Assert(segments_comparable(s2, s1));
  end Lemma_Segments_Comparable_Symmetric;

  procedure Lemma_Segments_Intersect_Implies_Intersection_Can_Subtract(s1: in segment_2d; s2: in segment_2d)
  is null;

  procedure Lemma_Can_Subtract_Overlap_Pts(p: point_2d; e, s: segment_2d)
  is begin
    -- Pre-condiitons
    pragma Assert(segments_comparable(e, s));
    pragma Assert(can_add(e.p1, vector_from_point_to_point(e.p1, e.p2)));
    pragma Assert(are_segments_overlapping(e, s));
    pragma Assert(can_subtract(p, e.p1));
    pragma Assert(can_subtract(p, e.p2));
    pragma Assert(can_subtract(p, s.p1));
    pragma Assert(can_subtract(p, s.p2));
    pragma Assert(can_subtract(e.p1, p));
    pragma Assert(can_subtract(e.p2, p));
    pragma Assert(can_subtract(s.p1, p));
    pragma Assert(can_subtract(s.p2, p));
    -- Post-conditions
    -- min_overlap_point(e, s) returns one of (e.p1, s.p1, s.p2))
    pragma Assert(can_subtract(p, min_overlap_point(e, s)));
    pragma Assert(can_subtract(min_overlap_point(e, s), p));
    -- min_overlap_point(e, s) returns one of (e.p2, s.p1, s.p2))
    pragma Assert(can_subtract(p, max_overlap_point(e, s)));
    pragma Assert(can_subtract(max_overlap_point(e, s), p));
  end Lemma_Can_Subtract_Overlap_Pts;

end Segment_Lemmas;
