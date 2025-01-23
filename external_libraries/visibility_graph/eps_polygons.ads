-- -----------------------------------------------------------------------------
-- eps_polygons.ads               Dependable Computing
-- Corresponds to logic from eps_polygons.pvs
-- -----------------------------------------------------------------------------
with vectors_2d;
with polygons_2d;
with eps_segments;
with segments_2d;
with perimeter_props;
with vertex_list;
with Finite_Sequences;
with Finseq_Fns;

package eps_polygons with SPARK_Mode is
  use vectors_2d;
  use polygons_2d;
  use eps_segments;
  use segments_2d;
  use perimeter_props;
  use vertex_list;

  --  % "//Make sure nonadjacent edges do not intersect."
  --  % Follows the pattern in polygons_2d.polygon_edges_do_not_cross?_alt.
  --  eps_polygon_edges_do_not_cross?(p: polygon_2d, eps: eps_type): bool =
  --    FORALL(i: below(p`num_vertices),
  --           j: {n: below(p`num_vertices) | n /= i}):
  --      LET ei = edges_of_polygon(p)(i), ej = edges_of_polygon(p)(j) IN
  --      IF equal_or_adjacent_edge?(p`num_vertices, i, j) THEN
  --        %% Might need to be generalized to exclude very small angles.
  --        NOT ^(ei) = -^(ej)
  --      ELSE
  --        distance(ei, ej) > eps
  --      ENDIF;
  function eps_polygon_edges_do_not_cross(p: polygon_2d; eps: eps_type) return Boolean is
    (for all i in 0 .. p.num_vertices-1 =>
       (for all j in 0 .. p.num_vertices-1 =>
          -- Corresponds to n /= i in PVS FORALL statement
          (i = j) or
            (if equal_or_adjacent_edge(p.num_vertices, i, j) then
                   not (normalize(segment_2d(edges_of_polygon(p, i))) = -normalize(segment_2d(edges_of_polygon(p, j))))
             else
               distance(edges_of_polygon(p, i), edges_of_polygon(p, j)) > eps)
       ));

  --  % Corresponds to visilibity.cpp function
  --  % bool Polygon::is_simple(double epsilon) const
  --  eps_polygon?(eps: eps_type)(p: polygon_2d): bool =
  --    (FORALL (i: below(p`num_vertices)):
  --       is_eps_line_segment?(eps)(edges_of_polygon(p)(i))) AND
  --    abs(area(p)) > eps AND
  --    % "//Make sure adjacent edges only intersect at a single point."
  --    (FORALL (i: below(p`num_vertices)):
  --       LET j = next_index(p,i),
  --           ei = edges_of_polygon(p)(i),
  --           ej = edges_of_polygon(p)(j) IN
  --       NOT intersection_returns_segment(ei, ej, eps)) AND
  --    % "//Make sure nonadjacent edges do not intersect."
  --    eps_polygon_edges_do_not_cross?(p, eps);
  function is_eps_polygon(p: polygon_2d; eps: eps_type) return Boolean is
    ((for all i in 0 .. p.num_vertices-1 =>
       is_eps_line_segment(eps, segment_2d(edges_of_polygon(p, i)))) and
       (abs(area(p)) > 0.0) and
         (for all i in 0 .. p.num_vertices-1 =>
              (not intersection_returns_segment(edges_of_polygon(p, i), edges_of_polygon(p, next_index(p, i)), eps))) and
    eps_polygon_edges_do_not_cross(p, eps));

  -- PVS:
  --  % Counter-clockwise epsilong square for arbitrarily large (or small) eps
  --  eps_square(eps: eps_type): polygon_2d =
  --    (#
  --      num_vertices := 4,
  --      vertices := LAMBDA(i: below(4)):
  --         IF    i = 0 THEN (# x := 0,     y := 0 #)
  --         ELSIF i = 1 THEN (# x := eps+1, y := 0 #)
  --         ELSIF i = 2 THEN (# x := eps+1, y := eps+1 #)
  --         ELSE             (# x := 0,     y := eps+1 #)
  --         ENDIF #);
  Eps_Square: constant polygon_2d :=
     (Num_Vertices => 4,
      Vertices => (0 => (X => 0.0, Y => 0.0),
                   1 => (X => Default_Eps + 1.0, Y => 0.0),
                   2 => (X => Default_Eps + 1.0, Y => Default_Eps + 1.0),
                   3 => (X => 0.0, Y => Default_Eps + 1.0),
                   others => (X => 0.0, Y => 0.0)));

  --  % Corresponds to while block in visilibity.cpp function
  --  % void Polygon::eliminate_redundant_vertices(double epsilon)
  --  % NB: called with a value of 1 in original code
  --  % tmp_list accumulates the partial result. Every vertex in tmp_list
  --  % must appear in an initial subsequence of the original vertex sequence.
  --  eliminate_redundant_vertices_helper(epsilon: eps_type,
  --      vertices: {f: uniq_vertex_seq | f`length > 3},
  --      % tmp_list needs to include something about third in it
  --      first: {i: nat | i <= vertices`length},
  --      tmp_list: {f: uniq_vertex_seq_with_empty |
  --                    f`length < vertices`length AND
  --                    contained_point_seq?(f, vertices^(1, first))},
  --      second: {i: nat | i > first AND i <= vertices`length+1 AND
  --                        tmp_list`length < i}
  --      ): RECURSIVE
  --        {f: uniq_vertex_seq_with_empty |
  --            f`length < vertices`length AND
  --            contained_point_seq?(f, vertices^(1, vertices`length - 1))} =
  --    LET third = second + 1 IN
  --    IF third > vertices`length THEN
  --      tmp_list
  --    ELSIF modget(vertices, first) = modget(vertices, third) THEN
  --      % This elsif is necessary to avoid TCC about segment_2d below
  --      % not being a valid segment
  --      eliminate_redundant_vertices_helper(epsilon, vertices,
  --        third, tmp_list, third+1)
  --    ELSE
  --      LET line: segment_2d =
  --       (# p1 := modget(vertices, first), p2 := modget(vertices, third) #) IN
  --      IF NOT is_eps_line_segment?(epsilon)(line) THEN
  --        eliminate_redundant_vertices_helper(epsilon, vertices,
  --          third, tmp_list, third+1)
  --      ELSIF distance(line, modget(vertices, second)) <= epsilon THEN
  --        eliminate_redundant_vertices_helper(epsilon, vertices,
  --          first, tmp_list, third)
  --      ELSE
  --        LET new_list = push_back(tmp_list, modget(vertices, second)) IN
  --        eliminate_redundant_vertices_helper(epsilon, vertices,
  --          second, new_list, third)
  --      ENDIF
  --    ENDIF
  --    MEASURE vertices`length - second + 1;
  --
  --  % Corresponds to visilibity.cpp function
  --  % void Polygon::eliminate_redundant_vertices(double epsilon)
  --  % NB: called with a value of 1 in original code
  --  eliminate_redundant_vertices(epsilon: eps_type,
  --                               vertices: {s: uniq_vertex_seq | 3 <= s`length}):
  --      {f: uniq_vertex_seq_with_empty | f`length <= vertices`length} =
  --    IF vertices`length < 4 THEN
  --      vertices
  --    ELSE
  --      % NB: All vertices get "rotated" by one, assuming no epsilon issues
  --      % I.e., the second vertex becomes the first vertex and the first vertex becomes the
  --      % last vertex
  --      LET vertices_temp = eliminate_redundant_vertices_helper(epsilon, vertices, 0, empty_seq, 1) IN
  --      % decide whether to add original first point
  --      IF (vertices_temp`length > 0) THEN
  --        LET dist =
  --          % Need to add this logic due to definition of segment_2d
  --          IF vertices_temp`seq(0) = vertices_temp`seq(vertices_temp`length-1) THEN
  --            distance(vertices_temp(0), vertices`seq(0))
  --          ELSE
  --            % TODO: Fix to use extra epsilon of 1
  --            LET line: segment_2d =
  --              (# p1 := vertices_temp`seq(0),
  --                 p2 := vertices_temp`seq(vertices_temp`length-1) #) IN
  --            distance(line, vertices`seq(0))
  --          ENDIF IN
  --        IF dist > epsilon THEN
  --          push_back(vertices_temp, vertices`seq(0))
  --        ELSE
  --          vertices_temp
  --        ENDIF
  --      ELSE
  --        vertices_temp
  --      ENDIF
  --    ENDIF;
  function eliminate_redundant_vertices(epsilon: eps_type; vertices: uniq_vertex_list) return uniq_vertex_list;

end eps_polygons;
