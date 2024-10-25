-- -----------------------------------------------------------------------------
-- between_rays.ads              Dependable Computing
-- Corresponds to logic from between_rays.pvs
-- -----------------------------------------------------------------------------
-- Build segments off of vectors and points.
with vectors_2d;
with vectors_cross_2d;
with segments_2d;

-- Declaration of types and functions on segments.
package between_rays with SPARK_Mode is
  use vectors_2d;
  use vectors_cross_2d;
  use segments_2d;

  -- From segments_2d.pvs:
  --  % Given that adjacent edges s = (u, v) and e = (v, w) share the common
  --  % vertex v, determine if a point p is "between" the rays that originate
  --  % at v and are collinear with the edges. This is based on an angular sweep
  --  % from (u, v) toward (v, w) in the counter-clockwise direction.
  --  between_rays?(s, e: segment_2d)(p: point_2d): bool =
  --    LET u = s`p1, v = e`p1, w = e`p2,
  --        uv = u - v, pv = p - v, wv = w - v IN
  --    IF cross(uv, wv) < 0
  --      THEN cross(uv, pv) > 0 OR cross(wv, pv) < 0    % wide angle case
  --      ELSE cross(uv, pv) > 0 AND cross(wv, pv) < 0
  --    ENDIF
  function is_between_rays(s: segment_2d; e: segment_2d; p: point_2d) return Boolean is
    (if (cross(s.p1 - e.p1, e.p2 - e.p1) < 0.0)
     then (cross(s.p1 - e.p1, p - e.p1) > 0.0 or cross(e.p2 - e.p1, p - e.p1) < 0.0)
     else (cross(s.p1 - e.p1, p - e.p1) > 0.0 and cross(e.p2 - e.p1, p - e.p1) < 0.0))
    with
      Pre => can_subtract(s.p1, e.p1) and can_subtract(p, e.p1);

end between_rays;
