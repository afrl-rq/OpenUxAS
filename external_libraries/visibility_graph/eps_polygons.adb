-- -----------------------------------------------------------------------------
-- eps_polygons.ads               Dependable Computing
-- Corresponds to logic from eps_polygons.pvs
-- -----------------------------------------------------------------------------
-- For polygons to be used in visibility graphs that have epsilon constraints
package body eps_polygons with
 SPARK_Mode
is

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
  function eliminate_redundant_vertices
   (epsilon : eps_type; vertices : uniq_vertex_list) return uniq_vertex_list
  is
    vertices_temp        : uniq_vertex_list := vertex_list.empty_seq;
    first, second, third : Natural;
  begin
    if (vertices.num_vertices < 4) then
      return vertices;
    else
      first  := 0;
      second := 1;
      third  := 2;
      eliminate_redundant_vertices_helper :
      while third <= vertices.num_vertices loop
        pragma Loop_Variant (Increases => third);
        declare
          line : segment_2d :=
           (p1 => vertices.vertices (first), p2 => vertices.vertices (third));
        begin
          if (is_eps_line_segment (epsilon, line)) then
            first  := third;
            second := third + 1;
            third  := third + 2;
            -- continue
          else
            if (distance (line, vertices.vertices (second)) <= epsilon) then
              second := third;
              third  := third + 1;
            else
              -- add vertices.vertices(second) to vertices_temp.vertices
              --   LET new_list = push_back(tmp_list, modget(vertices, second)) IN
              vertices_temp.vertices (vertices_temp.num_vertices) :=
               vertices.vertices (second);
              vertices_temp.num_vertices := vertices_temp.num_vertices + 1;
              first                      := second;
              second                     := third;
              third                      := third + 1;
            end if;
          end if;
        end;
      end loop eliminate_redundant_vertices_helper;
      if vertices_temp.num_vertices > 0 then
        --  LET dist =
        --    % Need to add this logic due to definition of segment_2d
        --    IF vertices_temp`seq(0) = vertices_temp`seq(vertices_temp`length-1) THEN
        --      distance(vertices_temp(0), vertices`seq(0))
        --    ELSE
        --      % TODO: Fix to use extra epsilon of 1
        --      LET line: segment_2d =
        --        (# p1 := vertices_temp`seq(0),
        --           p2 := vertices_temp`seq(vertices_temp`length-1) #) IN
        --      distance(line, vertices`seq(0))
        --    ENDIF IN
        declare
          line : segment_2d :=
           (p1 => vertices_temp.vertices (0),
            p2 => vertices_temp.vertices (vertices_temp.num_vertices - 1));
          dist : Float :=
           (if
             vertices_temp.vertices (0) =
             vertices_temp.vertices (vertices_temp.num_vertices - 1)
            then distance (vertices_temp.vertices (0), vertices.vertices (0))
            else distance (line, vertices.vertices (0)));
        begin
          if (dist > epsilon) then
            -- push_back(vertices_temp, vertices`seq(0))
            vertices_temp.vertices (vertices_temp.num_vertices) :=
             vertices.vertices (0);
            vertices_temp.num_vertices := vertices_temp.num_vertices + 1;
          end if;
        end;
      end if;
    end if;
    return vertices_temp;
  end eliminate_redundant_vertices;

end eps_polygons;
