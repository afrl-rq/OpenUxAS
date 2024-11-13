-- -----------------------------------------------------------------------------
-- segments_2d.ads              Dependable Computing
-- Corresponds to logic from segments_2d.pvs
-- -----------------------------------------------------------------------------
-- Build segments off of vectors and points.
with vectors_2d;
with vectors_cross_2d;
with floats;

-- Declaration of types and functions on segments.
package segments_2d with SPARK_Mode is
  use vectors_2d;
  use vectors_cross_2d;
  use floats;

  function segment_2d_weak_constraint(p1, p2: point_2d) return Boolean is
    (can_subtract(p2, p1) and then can_subtract(p1, p2)
     and then vector_2d_constraint(p2.x - p1.x, p2.y - p1.y)
     and then can_add(p1, vector_from_point_to_point(p1, p2))
     and then can_add(p2, vector_from_point_to_point(p2, p1))
     and then add_constraint(p1.x * p1.x,
         vector_from_point_to_point(p1, p2).x * vector_from_point_to_point(p1, p2).x)
     and then add_constraint(p2.x * p2.x,
         vector_from_point_to_point(p2, p1).x * vector_from_point_to_point(p2, p1).x)
     and then add_constraint(p1.y * p1.y,
         vector_from_point_to_point(p1, p2).y * vector_from_point_to_point(p1, p2).y)
     and then add_constraint(p2.y * p2.y,
         vector_from_point_to_point(p2, p1).y * vector_from_point_to_point(p2, p1).y)
    )
      with Ghost, Post=>True;
  -- From segments_2d.pvs:
  --   segment_2d: TYPE =
  --     [#
  --       p1: point_2d,
  --       p2: {p: point_2d | p /= p1}
  --      #];
  function segment_2d_constraint(p1, p2: point_2d) return Boolean is
    (p2 /= p1 and then can_subtract(p2, p1) and then can_subtract(p1, p2)
     -- TODO: can_subtract(p2, p1) => can_subtract(p1, p2), but the provers cannot
     -- seem to figure this out, so we're baking this into the predicate.
     and then norm(p2 - p1) > vector_float_eps -- FP retrenchment
     and then can_multiply(1.0/norm(p2 - p1), p2 - p1)
     -- TODO: The can_multiply predicate above should not be required, as it
     --   should always hold from the prior preconditions (see normalize)
     and then norm(p1 - p2) > vector_float_eps
     and then can_multiply(1.0/norm(p1 - p2), p1 - p2)
     and then vector_2d_constraint(p2.x - p1.x, p2.y - p1.y)
     and then can_add(p1, vector_from_point_to_point(p1, p2))
     and then can_add(p2, vector_from_point_to_point(p2, p1))
     and then can_add(p1.x * p1.x,
         vector_from_point_to_point(p1, p2).x * vector_from_point_to_point(p1, p2).x)
     and then can_add(p2.x * p2.x,
         vector_from_point_to_point(p2, p1).x * vector_from_point_to_point(p2, p1).x)
     and then can_add(p1.y * p1.y,
         vector_from_point_to_point(p1, p2).y * vector_from_point_to_point(p1, p2).y)
     and then can_add(p2.y * p2.y,
         vector_from_point_to_point(p2, p1).y * vector_from_point_to_point(p2, p1).y)
    )
    with Ghost, Post=>True;

  -- For degenerate cases where me might want to allow points
  -- to be the same
  type weak_segment_2d is record
    p1: point_2d;
    p2: point_2d;
  end record;

  -- From segments_2d.pvs:
  --   segment_2d: TYPE =
  --     [#
  --       p1: point_2d,
  --       p2: {p: point_2d | p /= p1}
  --      #];
  -- A 2D segment.
  subtype segment_2d is weak_segment_2d with
    Ghost_Predicate => segment_2d_constraint(segment_2d.p1, segment_2d.p2);

  function Real_Eq(f1, f2 : segment_2d) return Boolean with
    Global => null,
    Ghost,
    Import,
    Annotate => (GNATprove, Logical_Equal);

  --  segment_intersection_type: TYPE =
  --    {Collinear_Overlapping,
  --     Collinear_Non_Overlapping,
  --     Parallel,
  --     Intersecting,
  --     Non_Parallel_Not_Intersecting};
  type segment_intersection_type is
    (Collinear_Overlapping,
     Collinear_Non_Overlapping,
     Parallel,
     Intersecting,
     Non_Parallel_Not_Intersecting);

  -- From segments_2d.pvs:
  --   is_point_on_segment?(p: point_2d, s: segment_2d): bool =
  --     LET A: point_2d = s`p1 IN
  --     LET B: point_2d = s`p2 IN
  --     LET C: point_2d = p IN
  --     LET A_to_C: vector_2d = vector_from_point_to_point(A, C) in
  --     LET A_to_B: vector_2d = vector_from_point_to_point(A, B) in
  --     LET AB_dot_AC: real = A_to_B * A_to_C IN
  --     LET AB_dot_AB: real = A_to_B * A_to_B IN
  --     are_vectors_collinear?(A_to_C, A_to_B) AND
  --     (0 <= AB_dot_AC) AND (AB_dot_AC <= AB_dot_AB);
  -- Return true if the given point is on the given segment, else false
  -- Note that is_point_on_segment? has been proven to be functionally
  -- identical to point_on_segment? through the following lemma:
  --  point_on_segment?_iff_is_point_on_segment?: LEMMA
  --    FORALL(p: point_2d, s: segment_2d):
  --      point_on_segment?(p, s) IFF is_point_on_segment?(p, s)
  function is_point_on_segment(p: point_2d;
                               s: segment_2d) return Boolean
  is
    -- Literal copy of body from segments_2d.pvs:
    --  (are_vectors_collinear?(A_to_C, A_to_B) and
    --   (0 <= AB_dot_AC) and (AB_dot_AC <= AB_dot_AB))
    -- Substitute for AB_dot_AC and AB_dot_AB:
    --  (are_vectors_collinear?(A_to_C, A_to_B) and
    --   (0 <= A_to_B * A_to_C) and (A_to_B * A_to_C <= A_to_B * A_to_B))
    -- Substitute for A_to_C and A_to_B and break lines/indent:
    --  (are_vectors_collinear?(vector_from_point_to_point(A, C),
    --                          vector_from_point_to_point(A, B)) and
    --   (0 <= vector_from_point_to_point(A, B) * vector_from_point_to_point(A, C)) and
    --    (vector_from_point_to_point(A, B) * vector_from_point_to_point(A, C) <=
    --     vector_from_point_to_point(A, B) * vector_from_point_to_point(A, B)))
    -- Substitute for A, B, and C, and remove ?:
    (are_vectors_collinear(vector_from_point_to_point(s.p1, p),
     vector_from_point_to_point(s.p1, s.p2)) and
       (0.0 <= vector_from_point_to_point(s.p1, s.p2) * vector_from_point_to_point(s.p1, p)) and
         (vector_from_point_to_point(s.p1, s.p2) * vector_from_point_to_point(s.p1, p) <=
                vector_from_point_to_point(s.p1, s.p2) * vector_from_point_to_point(s.p1, s.p2)))
        with
          Pre => can_subtract(p, s.p1);

  --   is_point_on_segment_exclusive?(p: point_2d, s: segment_2d): bool =
  --     is_point_on_segment(p, s) AND NOT
  --     (p = s`p1 OR p = s`p2);
  function is_point_on_segment_exclusive(p: point_2d; s: segment_2d) return Boolean is
    (is_point_on_segment(p, s) and not
         (p = s.p1 or p = s.p2))
      with
        Pre => can_subtract(p, s.p1);

  -----------------------------------------------------------------------------
  -- begin retrenchment section 1
  -----------------------------------------------------------------------------
  function can_subtract(p: Vect2; s: weak_segment_2d) return Boolean is
    (can_subtract(p, s.p1) and can_subtract(p, s.p2) and
         can_subtract(s.p1, p) and can_subtract(s.p2, p))
      with Ghost, Post=>True;
  function can_subtract(s1: in weak_segment_2d; s2: in weak_segment_2d) return Boolean is
    (can_subtract(s1.p1, s2.p1) and then can_subtract(s1.p1, s2.p2) and then
     can_subtract(s1.p2, s2.p1) and then can_subtract(s1.p2, s2.p2))
      with Ghost, Post=>True;
  function segments_weak_comparable(s1: in weak_segment_2d; s2: in weak_segment_2d) return Boolean is
    (can_subtract(s1, s2) and then can_subtract(s2, s1) and then
     segment_2d_weak_constraint(s1.p1, s1.p2) and then segment_2d_weak_constraint(s2.p1, s2.p2) and then
     can_add(s1.p1, vector_from_point_to_point(s1.p1, s1.p2)) and then
     can_add(s1.p1.x * s1.p1.x, vector_from_point_to_point(s1.p1, s1.p2).x * vector_from_point_to_point(s1.p1, s1.p2).x) and then
     can_add(s1.p1.y * s1.p1.y, vector_from_point_to_point(s1.p1, s1.p2).y * vector_from_point_to_point(s1.p1, s1.p2).y))
    with Ghost, Post=>True;
  function segments_comparable(s1: in weak_segment_2d; s2: in weak_segment_2d) return Boolean is
    (can_subtract(s1, s2) and then can_subtract(s2, s1) and then
     segment_2d_constraint(s1.p1, s1.p2) and then segment_2d_constraint(s2.p1, s2.p2) and then
     can_add(s1.p1, vector_from_point_to_point(s1.p1, s1.p2)) and then
     can_add(s1.p1.x * s1.p1.x, vector_from_point_to_point(s1.p1, s1.p2).x * vector_from_point_to_point(s1.p1, s1.p2).x) and then
     can_add(s1.p1.y * s1.p1.y, vector_from_point_to_point(s1.p1, s1.p2).y * vector_from_point_to_point(s1.p1, s1.p2).y))
    with Ghost, Post=>True;
  -----------------------------------------------------------------------------
  -- end retrenchment section 1
  -----------------------------------------------------------------------------

  -- From segments_2d.pvs:
  --  reverse_segment(s: segment_2d): segment_2d = (# p1 := s`p2, p2 := s`p1 #)
  function reverse_segment(s: in segment_2d) return segment_2d is
    (segment_2d'(p1 => s.p2, p2 => s.p1));

  -- From segemtns_2d.pvs (body will be shown in segments_2d.adb):
  --   segment_intersect_kernel(s1, s2: segment_2d):
  --      [segment_intersection_type, point_2d] = <body>
  type intersect_kernel_result is record
    intersect_result: segment_intersection_type;
    intersect_location: point_2d;
  end record;
  function segment_intersect_kernel(s1, s2: in segment_2d) return intersect_kernel_result
    with Pre => segments_comparable(s1, s2);

  --  are_segments_intersecting?(s1: segment_2d)(s2: segment_2d): bool =
  --    (segment_intersect_kernel(s1, s2)`1 = Intersecting or
  --     segment_intersect_kernel(s1, s2)`1 = Collinear_Overlapping);
  function are_segments_intersecting(s1, s2: in segment_2d) return Boolean is
    ((segment_intersect_kernel(s1, s2).intersect_result = Intersecting) or
       (segment_intersect_kernel(s1, s2).intersect_result = Collinear_Overlapping))
      with Pre => segments_comparable(s1, s2);

  -- From segments_2d.pvs:
  --  % We provide a variant of this function for the case where the first
  --  % point of each segment is the common one.
  --  % Segments s = (u, v) and e = (u, w) share the common point u.
  --  point_between_rays?(s, e: segment_2d)(p: point_2d): bool =
  --    LET u = s`p1, v = s`p2, w = e`p2,
  --        vu = v - u, pu = p - u, wu = w - u IN
  --    IF cross(vu, wu) < 0
  --      THEN cross(vu, pu) > 0 OR cross(wu, pu) < 0    % wide angle case
  --      ELSE cross(vu, pu) > 0 AND cross(wu, pu) < 0
  --    ENDIF
  function is_point_between_rays(s: segment_2d; e: segment_2d; p: point_2d) return Boolean is
    (if (cross(s.p2 - s.p1, e.p2 - s.p1) < 0.0)
     then (cross(s.p2 - s.p1, p - s.p1) > 0.0 or cross(e.p2 - s.p1, p - s.p1) < 0.0)
     else (cross(s.p2 - s.p1, p - s.p1) > 0.0 and cross(e.p2 - s.p1, p - s.p1) < 0.0))
    with
      Pre => can_subtract(e.p2, s.p1) and can_subtract(p, s.p1);

  --  ^(s: segment_2d): Normalized = ^(s`p2 - s`p1);
  function normalize(s: segment_2d) return Vect2 is
    (normalize(s.p2 - s.p1));

  --  segments_share_an_endpoint(s1, s2: segment_2d): boolean =
  --    (s1`p1 = s2`p1) OR (s1`p1 = s2`p2) OR
  --      (s1`p2 = s2`p1) OR (s1`p2 = s2`p2);
  function segments_share_an_endpoint(s1, s2: segment_2d) return Boolean is
    ((s1.p1 = s2.p1) OR (s1.p1 = s2.p2) OR
      (s1.p2 = s2.p1) OR (s1.p2 = s2.p2));

end segments_2d;
