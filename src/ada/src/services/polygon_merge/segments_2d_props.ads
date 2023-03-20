-- -----------------------------------------------------------------------------
-- segments_2d_props.ads              Dependable Computing
-- Corresponds to logic from segments_2d_props.pvs
-- -----------------------------------------------------------------------------
-- Build segments off of vectors and points.
with vectors_2d;
with vectors_cross_2d;
with segments_2d;

-- Declaration of types and functions on segments.
package segments_2d_props with SPARK_Mode is
  use vectors_2d;
  use vectors_cross_2d;
  use segments_2d;

  --    are_segments_overlapping?(s1: segment_2d)(s2: segment_2d): bool =
  --      EXISTS (p1, p2: point_2d):
  --        p1 /= p2 AND
  --        point_on_segment?(p1, s1) AND point_on_segment?(p2, s1) AND
  --        point_on_segment?(p1, s2) AND point_on_segment?(p2, s2)
  --  ...
  --    kernel_eq_segments_overlapping_unless_shared_endpt: LEMMA
  --      FORALL (s1, s2: segment_2d):
  --        (NOT segments_share_an_endpoint(s1, s2)) IMPLIES
  --          (are_segments_overlapping?(s1)(s2) IFF
  --              segment_intersect_kernel(s1, s2)`1 = Collinear_Overlapping);
  function are_segments_overlapping(s1, s2: segment_2d) return Boolean is
    (not segments_share_an_endpoint(s1, s2) and
         segment_intersect_kernel(s1, s2).intersect_result = Collinear_Overlapping)
      with Pre => segments_comparable(s1, s2) and then
      can_add(s1.p1, vector_from_point_to_point(s1.p1, s1.p2));

  --  % Closest overlapping point to the initial vertex of s1
  --  min_overlap_point(s1: segment_2d,
  --                    s2: (are_segments_overlapping?(s1))): point_2d =
  --    IF point_on_segment?(s1`p1, s2) THEN s1`p1
  --    ELSIF ^(s1`p2 - s1`p1) = ^(s2`p2 - s2`p1) THEN s2`p1
  --    ELSE s2`p2
  --    ENDIF
  function min_overlap_point(s1, s2: segment_2d) return point_2d is
    (if is_point_on_segment(s1.p1, s2) then s1.p1
     elsif normalize(s1) = normalize(s2) then s2.p1
     else s2.p2)
  with
      Pre => segments_comparable(s1, s2)
    and then can_add(s1.p1, vector_from_point_to_point(s1.p1, s1.p2))
    and then (are_segments_overlapping(s1, s2));

  --  % Farthest overlapping point from the initial vertex of s1
  --  max_overlap_point(s1: segment_2d,
  --                    s2: (are_segments_overlapping?(s1))): point_2d =
  --    IF point_on_segment?(s1`p2, s2) THEN s1`p2
  --    ELSIF ^(s1`p2 - s1`p1) = ^(s2`p2 - s2`p1) THEN s2`p2
  --    ELSE s2`p1
  --    ENDIF
  function max_overlap_point(s1, s2: segment_2d) return point_2d is
    (if is_point_on_segment(s1.p2, s2) then s1.p2
     elsif normalize(s1) = normalize(s2) then s2.p2
     else s2.p1)
  with
      Pre => segments_comparable(s1, s2)
    and then can_add(s1.p1, vector_from_point_to_point(s1.p1, s1.p2))
    and then (are_segments_overlapping(s1, s2));

end segments_2d_props;
