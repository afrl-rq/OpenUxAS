-- -----------------------------------------------------------------------------
-- eps_segments.ads               Dependable Computing
-- Corresponds to logic from eps_segments.pvs
-- -----------------------------------------------------------------------------
-- For segments to be used in visibility graphs that have epsilon constraints
with prelude;
with floats;
with vectors_2d;
with vertex_list;
with segments_2d;
with vectors_cross_2d;

--Incompatible with SPARK_Mode
-- generic eps_pkg: in prelude.nn_float;

package eps_segments with SPARK_Mode is
   use vectors_2d;
   use vertex_list;
   use prelude;
   use segments_2d;
   use vectors_cross_2d;

   -- PVS:
   --  point: NONEMPTY_TYPE = vector_2d;
   subtype point is vector_2d;

   -- PVS:
   --  modget(vertices: {f: finseq[point] | f`length > 0}, i: nat): point =
   --    vertices`seq(rem(vertices`length)(i));
   function modget(vertices: bounded_vertex_list; i: Natural) return point is
     (vertices.vertices(i rem vertices.num_vertices))
       with
         Pre => vertices.num_vertices > 0;

   -- PVS:
   --  eps_type: NONEMPTY_TYPE = nnreal;
   subtype eps_type is nn_float;
   Default_Eps: constant eps_type := 0.00000001; -- Inferred from original C++ code, but not in the PVS spec
   -- Not guaranteed to be an eps_segment for an arbitrary value of epsilon
   Default_Segment: constant segment_2d :=
     (p1 => zero_point,
      p2 => (x => 0.0, y => Default_Eps + 1.0));

   function distance_pre(p1, p2: point) return Boolean is
     (can_add(p1.x, -p2.x) and then can_add(p1.y, -p2.y) and then
      vector_2d_constraint(p1.x - p2.x, p1.y - p2.y))
       with Ghost;

   -- PVS:
   --  %% Move to vectors_2d?
   --  % Corresponds to visilibity.cpp function
   --  % double distance(const Point& point1, const Point& point2)
   --  distance(p1, p2: point): nnreal =
   --    norm(p1 - p2);
   function distance(p1, p2: point) return nn_float is
     (norm(p1 - p2))
       with
         Pre => distance_pre(p1, p2);

   --  % Corresponds to visilibity.cpp function
   --  % double Line_Segment::length() const
   --  length(s: segment_2d): nnreal =
   --    distance(s`p1, s`p2);
   function length(s: segment_2d) return nn_float is
     (distance(s.p1, s.p2))
       with
         Pre => distance_pre(s.p1, s.p2);

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
   function projection_onto(p: point; s: segment_2d) return point;

   function distance_pre(p: point; s: segment_2d) return Boolean is
     (distance_pre(p, projection_onto(p, s)))
     with Ghost;

   function distance_pre(s: segment_2d; p: point) return Boolean is
     (distance_pre(projection_onto(p, s), p))
     with Ghost;

   -- PVS:
   --  % Corresponds to visilibity.cpp function
   --  % double distance(const Point& pointemp,
   --  %         const Line_Segment& line_segment_temp)
   --  distance(p: point, s: segment_2d): nnreal =
   --    distance(p, projection_onto(p, s));
   function distance(p: point; s: segment_2d) return nn_float is
     (distance(p, projection_onto(p, s)))
     with
       Pre => distance_pre(p, s);

   -- PVS:
   --  % Corresponds to visilibity.cpp function
   --  % double distance(const Line_Segment& line_segment_temp,
   --  %         const Point& pointemp)
   --  distance(s: segment_2d, p: point): nnreal =
   --    distance(p, s);
   function distance(s: segment_2d; p: point) return nn_float is
     (distance(projection_onto(p, s), p))
     with
       Pre => distance_pre(s, p);

   -- PVS:
   --  % Corresponds to visilibity.cpp function
   --  % double distance(const Line_Segment& line_segment1,
   --  %         const Line_Segment& line_segment2)
   --  distance(s1, s2: segment_2d): nnreal =
   --    IF are_segments_intersecting?(s1)(s2) THEN
   --      0
   --    ELSE
   --      LET s1p1dist = distance(s1`p1, s2),
   --          s1p2dist = distance(s1`p2, s2),
   --          s2p1dist = distance(s2`p1, s1),
   --          s2p2dist = distance(s2`p2, s1) IN
   --      min(min(min(s1p1dist, s1p2dist), s2p1dist), s2p2dist)
   --    ENDIF;
   function distance(s1, s2: segment_2d) return nn_float is
     (if are_segments_intersecting(s1, s2) then 0.0 else
           min(min(min(distance(s1.p1, s2), distance(s1.p2, s2)),
        distance(s2.p1, s1)), distance(s2.p2, s1)))
     with
       Pre => distance_pre(s1.p1, s2) and distance_pre(s1.p2, s2) and
     distance_pre(s2.p1, s1) and distance_pre(s2.p2, s1);

   -- PVS:
   --  % Corresponds to visilibity.cpp function
   --  % bool intersect_proper(const Line_Segment& line_segment1,
   --  %           const Line_Segment& line_segment2, double epsilon)
   --  % Holds only when there is an intersection and the intersection point
   --  % is farther than epsilon from each of the four endpoints.
   --  intersect_proper(s1, s2: segment_2d, eps: eps_type): bool =
   --    LET a = s1`p1, b = s1`p2, c = s2`p1, d = s2`p2 IN
   --    LET min_dist = min(min(min(distance(a, s2), distance(b, s2)), distance(c, s1)), distance(d, s1)) IN
   --    IF min_dist <= eps THEN
   --      FALSE
   --    ELSIF cross(b - a, c - b) * cross(b - a, d - b) < 0 AND
   --          cross(d - c, b - d) * cross(d - c, a - d) < 0 THEN
   --      TRUE
   --    ELSE
   --      FALSE
   --    ENDIF;
   function intersect_proper(s1, s2: segment_2d; eps: eps_type) return Boolean is
     (if (min(min(min(distance(s1.p1, s2), distance(s1.p2, s2)), distance(s2.p1, s1)), distance(s2.p2, s1)) <= eps) then False
      elsif (cross(s1.p2 - s1.p1, s2.p1 - s1.p2) * cross(s1.p2 - s1.p1, s2.p2 - s1.p2) < 0.0 and
          cross(s2.p2 - s2.p1, s1.p2 - s2.p2) * cross(s2.p2 - s2.p1, s1.p1 - s2.p2) < 0.0) then True
      else False)
     with
       Pre => distance_pre(s1.p1, s2) and distance_pre(s1.p2, s2) and
     distance_pre(s2.p1, s1) and distance_pre(s2.p2, s1);

   -- PVS:
   --  intersect(s1, s2: segment_2d, eps: eps_type): bool =
   --    distance(s1, s2) <= eps;
   function intersect(s1, s2: segment_2d; eps: eps_type) return Boolean is
     (distance(s1, s2) <= eps)
       with
         Pre => distance_pre(s1.p1, s2) and distance_pre(s1.p2, s2) and
       distance_pre(s2.p1, s1) and distance_pre(s2.p2, s1);

   -- PVS:
   --  % Captures key feature of intersection code in visilibity.cpp
   --  %   Line_Segment intersection(const Line_Segment& line_segment1,
   --  %              const Line_Segment& line_segment2, double epsilon)
   --  % Returns true if and only if that "intersection" returns a line
   --  % segment with two points
   --  intersection_returns_segment(s1, s2: segment_2d, eps: eps_type): bool =
   --    IF NOT intersect(s1, s2, eps) THEN
   --      FALSE
   --    ELSIF intersect_proper(s1, s2, eps) THEN
   --      FALSE
   --    ELSE
   --      LET a = s1`p1, b = s1`p2, c = s2`p1, d = s2`p2,
   --          dist_a = distance(a, s2), dist_b = distance(b, s2),
   --          dist_c = distance(c, s1), dist_d = distance(d, s1) IN
   --      (dist_a <= eps AND dist_b <= eps AND distance(a, b) > eps) OR
   --      (dist_c <= eps AND dist_d <= eps AND distance(c, d) > eps) OR
   --      (dist_a <= eps AND dist_c <= eps AND distance(a, c) > eps) OR
   --      (dist_a <= eps AND dist_d <= eps AND distance(a, d) > eps) OR
   --      (dist_b <= eps AND dist_c <= eps AND distance(b, c) > eps) OR
   --      (dist_b <= eps AND dist_d <= eps AND distance(b, d) > eps)
   --    ENDIF;
   function intersection_returns_segment(s1, s2: segment_2d; eps: eps_type) return Boolean is
     (if not intersect(s1, s2, eps) then False
      elsif intersect_proper(s1, s2, eps) then False
      else
        (distance(s1.p1, s2) <= eps AND distance(s1.p2, s2) <= eps AND distance(s1.p1, s1.p2) > eps) OR
          (distance(s2.p1, s1) <= eps AND distance(s2.p2, s1) <= eps AND distance(s2.p1, s2.p2) > eps) OR
        (distance(s1.p1, s2) <= eps AND distance(s2.p1, s1) <= eps AND distance(s1.p1, s2.p1) > eps) OR
          (distance(s1.p1, s2) <= eps AND distance(s2.p2, s1) <= eps AND distance(s1.p1, s2.p2) > eps) OR
        (distance(s1.p2, s2) <= eps AND distance(s2.p1, s1) <= eps AND distance(s1.p2, s2.p1) > eps) OR
          (distance(s1.p2, s2) <= eps AND distance(s2.p2, s1) <= eps AND distance(s1.p2, s2.p2) > eps)
     )
       with
         Pre => distance_pre(s1.p1, s2) and distance_pre(s1.p2, s2) and
       distance_pre(s2.p1, s1) and distance_pre(s2.p2, s1) and
     distance_pre(s1.p1, s2.p1) and distance_pre(s1.p1, s2.p2) and
     distance_pre(s1.p2, s2.p1) and distance_pre(s1.p2, s2.p2);

   -- PVS:
   --  % Corresponds to visilibity's Line_segment::Line_Segment(Point, Point, double)
   --  % NB: In visilibity, line segments that fail this end up being "line segments"
   --  % with a single point: the starting point
   --  is_eps_line_segment?(eps: eps_type)(seg: segment_2d): bool =
   --    length(seg) > eps;
   function is_eps_line_segment(eps: eps_type; seg: segment_2d) return Boolean is
     (length(seg) > eps);

   -- PVS:
   --  % NB: eps can be 0
   --  eps_line_segment(eps: eps_type): NONEMPTY_TYPE = (is_eps_line_segment?(eps))
   --    CONTAINING (# p1 := (# x := 0, y := 0 #),
   --                  p2 := (# x := eps + 1, y := eps #) #);
   -- Incompatible with SPARK_Mode
   --  subtype eps_line_segment is segment_2d
   --    with Ghost_Predicate => is_eps_line_segment(eps_pkg, eps_line_segment);

end eps_segments;
