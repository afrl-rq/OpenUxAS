-- -----------------------------------------------------------------------------
-- segments_2d.adb              Dependable Computing
-- Corresponds to logic from segments_2d.pvs
-- -----------------------------------------------------------------------------
with floats;
with prelude;
with Lemmas;

package body segments_2d with SPARK_MODE is
  use floats;
  use prelude;
  use Lemmas;

  --  segment_intersect_kernel(s1, s2: segment_2d):
  --      [segment_intersection_type, point_2d] =
  --    LET p: point_2d = s1`p1 IN
  --    LET r: vector_2d = vector_from_point_to_point(s1`p1, s1`p2) IN
  --    LET q: point_2d = s2`p1 IN
  --    LET s: vector_2d = vector_from_point_to_point(s2`p1, s2`p2) IN
  --    LET r_cross_s: real = cross(r, s) IN
  --    LET q_minus_p_cross_r: real = cross((q - p), r) IN
  --    IF ((r_cross_s = 0) AND (q_minus_p_cross_r = 0)) THEN
  --      LET t0: real = (s2`p1 - s1`p1) * r IN
  --      LET t1: real = (s2`p2 - s1`p1) * r IN
  --      LET norm_sq: nnreal = r * r IN
  --      IF ((0 <= t0 AND t0 <= norm_sq) OR (0 <= t1 AND t1 <= norm_sq)) THEN
  --        (Collinear_Overlapping, zero_point)
  --      ELSE
  --        (Collinear_Non_Overlapping, zero_point)
  --      ENDIF
  --    ELSIF ((r_cross_s = 0) AND (q_minus_p_cross_r /= 0)) THEN
  --      (Parallel, zero_point)
  --    ELSE
  --      LET q_minus_p_cross_s: real = cross((q - p), s) IN
  --      LET t: real = q_minus_p_cross_s / r_cross_s IN
  --      LET u: real = q_minus_p_cross_r / r_cross_s IN
  --      IF ((0 <= t AND t <= 1) AND (0 <= u AND u <= 1)) THEN
  --        (Intersecting, mk_vect2(p`x + t * r`x, p`y + t * r`y))
  --      ELSE
  --        (Non_Parallel_Not_Intersecting, zero_point)
  --      ENDIF
  --    ENDIF;
  function segment_intersect_kernel(s1, s2: in segment_2d) return intersect_kernel_result is
    p: point_2d := s1.p1;
    r: vector_2d := vector_from_point_to_point(s1.p1, s1.p2);
    q: point_2d := s2.p1;
    s: vector_2d := vector_from_point_to_point(s2.p1, s2.p2);
    r_cross_s: Float := cross(r, s);
    q_minus_p_cross_r: Float := cross((q - p), r);
    result: intersect_kernel_result;
    t0, t1: Float;
    norm_sq: nn_float;
    q_minus_p_cross_s, t_by_r_cross_s, u_by_r_cross_s: Float;
    ui_factor: ui_float;
  begin
    if ((r_cross_s = 0.0) and (q_minus_p_cross_r = 0.0)) then
      t0 := (s2.p1 - s1.p1) * r;
      t1 := (s2.p2 - s1.p1) * r;
      norm_sq := r * r;
      if ((0.0 <= t0 and t0 <= norm_sq) or (0.0 <= t1 and t1 <= norm_sq)) then
        result := (intersect_result => Collinear_Overlapping,
                   intersect_location => zero_point);
      else
        result := (intersect_result => Collinear_Non_Overlapping,
                   intersect_location => zero_point);
      end if;
    elsif ((r_cross_s = 0.0) and (q_minus_p_cross_r /= 0.0)) then
      result := (intersect_result => Parallel, intersect_location => zero_point);
    else
      q_minus_p_cross_s := cross((q - p), s);
      -- Renaming and cross-multiplying r_cross_s to eliminate FP overflow concerns
      t_by_r_cross_s := q_minus_p_cross_s;
      u_by_r_cross_s := q_minus_p_cross_r;
      if ((0.0 <= t_by_r_cross_s AND t_by_r_cross_s <= r_cross_s) AND
            (0.0 <= u_by_r_cross_s AND u_by_r_cross_s <= r_cross_s)) THEN
        -- At this point, we know that 0 <= t_by_r_cross_s / r_cross_s <= 1
        pragma Assert(0.0 <= t_by_r_cross_s / r_cross_s);
        pragma Assert(t_by_r_cross_s / r_cross_s <= 1.0);
        -- Making this explicit to make it easier for the prover
        ui_factor := t_by_r_cross_s / r_cross_s; -- unit interval factor
        -- Follows from last half of function precondition:
        -- can_add(s1.p1, vector_from_point_to_point(s1.p1, s1.p2))
        pragma Assert(can_add(p, r));
        pragma Assert(can_add(p.x, r.x));
        pragma Assert(can_add(p.y, r.y));
        pragma Assert(vector_2d_constraint(p.x, p.y));
        pragma Assert(multiply_constraint(p.x, p.x));
        pragma Assert(multiply_constraint(p.y, p.y));
        pragma Assert(vector_2d_constraint(r.x, r.y));
        pragma Assert(multiply_constraint(r.x, r.x));
        pragma Assert(multiply_constraint(r.y, r.y));
        pragma Assert(can_add(p, r)); -- already proved on line 79
        -- verbatim copy of can_add, but with p1 -> p, p2 -> r:
        pragma Assert(can_add(p.x, r.x) and then can_add(p.y, r.y) and then
                      vector_2d_constraint(p.x + r.x, p.y + r.y));
        -- Duplicate of first part of prior conjunction assert
        pragma Assert(can_add(p.x, r.x));
        pragma Assert((p.x + r.x) >= vector_float_type'First);
        pragma Assert((p.x + r.x) <= vector_float_type'Last);
        -- Duplicate of middle part of prior conjunction assert
        pragma Assert(can_add(p.y, r.y));
        pragma Assert((p.y + r.y) >= vector_float_type'First);
        pragma Assert((p.y + r.y) <= vector_float_type'Last);
        -- Duplicate of last part of prior conjunction assert
        pragma Assert(vector_2d_constraint(p.x + r.x, p.y + r.y));
        -- verbatim copy of vector_2d_constraint, but with x -> p.x + r.x, y -> p.y + r.y:
        pragma Assert(multiply_constraint(p.x + r.x, p.x + r.x) and then multiply_constraint(p.y + r.y, p.y + r.y) and then
                      add_constraint((p.x + r.x) * (p.x + r.x), (p.y + r.y) * (p.y + r.y)));
        -- Duplicate of first part of prior conjunction assert
        pragma Assert(multiply_constraint(p.x + r.x, p.x + r.x));
        -- Duplicate of middle part of prior conjunction assert
        pragma Assert(multiply_constraint(p.y + r.y, p.y + r.y));
        -- Duplicate of last part of above conjunction
--        pragma Assert(add_constraint((p.x + r.x) * (p.x * r.x), (p.y + r.y) * (p.y * r.y)));
        pragma Assert(multiply_constraint(p.x, p.x)); -- can multiply to fit in float
        pragma Assert(multiply_constraint(r.x, r.x));
        pragma Assert(can_add(p.x * p.x, r.x * r.x)); -- can multiply to fit in vector_float_type
        pragma Assert(multiply_constraint(p.y, p.y));
        pragma Assert(multiply_constraint(r.y, r.y));
        pragma Assert(can_add(p.y * p.y, r.y * r.y));
        pragma Assert(vector_2d_constraint(p.x, p.y));
        pragma Assert(vector_2d_constraint(p.x + r.x, p.y + r.y));
        Lemma_Between_Two_Values_With_Sum(p.x, r.x, ui_factor);
        pragma Assert(vector_float_type'First <= p.x + ui_factor * r.x);
        pragma Assert(p.x + ui_factor * r.x <= vector_float_type'Last);
        Lemma_Between_Two_Values_With_Sum(p.y, r.y, ui_factor);
        pragma Assert(vector_float_type'First <= p.y + ui_factor * r.y);
        pragma Assert(p.y + ui_factor * r.y <= vector_float_type'Last);
        -- The following "axiom" will be proven externally via PVS
        pragma Assume(vector_2d_constraint(p.x + ui_factor * r.x, p.y + ui_factor * r.y));
        result := (intersect_result => Intersecting, 
                   intersect_location => (x => p.x + ui_factor * r.x,
                                          y => p.y + ui_factor * r.y));
      else
        result := (intersect_result => Non_Parallel_Not_Intersecting, 
                   intersect_location => zero_point);
      end if;
    end if;
    return result;
  end segment_intersect_kernel;

end segments_2d;
