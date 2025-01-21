-- -----------------------------------------------------------------------------
-- eps_segments.ads               Dependable Computing
-- Corresponds to logic from eps_segments.pvs
-- -----------------------------------------------------------------------------
-- For segments to be used in visibility graphs that have epsilon constraints
package body eps_segments with SPARK_Mode is

  --  % Corresponds to visilibity.cpp function
  --  % Point Point::projection_onto(const Line_Segment& line_segment_temp) const
  --  projection_onto(p: point, s: segment_2d): point =
  --    %% Idea: generate a system of two equations from the prerequisites
  --    %% of a point projected on to a line segment and solve for the unknown
  --    %% value t:
  --    %%
  --    %%               * (A)
  --    %%               |
  --    %%               |
  --    %% (B) ==========*========== (C)
  --    %%    |- - t - -|
  --    %%              (D)
  --    %% The dot product of AD and CB is 0 because they are perpendicular
  --    %% and we can represent BC as a linear combination of BC = B + t(C-B).
  --    %%
  --    %% A is point p and BC is segment s.
  --    LET t =
  --      ((s`p2`x - p`x) * (s`p2`x - s`p1`x)
  --       + (s`p2`y - p`y) * (s`p2`y - s`p1`y))
  --      / (sq(s`p2`x - s`p1`x) + sq(s`p2`y - s`p1`y)) IN
  --    IF (0 <= t AND t <= 1) THEN
  --      t * s`p1 + (1 - t) * s`p2
  --    ELSIF distance(p, s`p1) < distance(p, s`p2) THEN
  --      s`p1
  --    ELSE
  --      s`p2
  --    ENDIF;
  function projection_onto(p: point; s: segment_2d) return point
  is
    t: float := ((s.p2.x - p.x) * (s.p2.x - s.p1.x)
                 + (s.p2.y - p.y) * (s.p2.y - s.p1.y))
      / (sq(s.p2.x - s.p1.x) + sq(s.p2.y - s.p1.y));
    result: point;
  begin
    if (0.0 <= t and t <= 1.0) then
      result := t * s.p1 + (1.0 - t) * s.p2;
    elsif distance(p, s.p1) < distance(p, s.p2) then
      result := s.p1;
    else
      result := s.p2;
    end if;
    return result;
  end projection_onto;

end eps_segments;
