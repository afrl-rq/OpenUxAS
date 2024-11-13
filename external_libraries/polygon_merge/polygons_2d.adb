-- -----------------------------------------------------------------------------
-- polygons_2d.adb              Dependable Computing
-- Corresponds to logic from polygons_2d.pvs
-- -----------------------------------------------------------------------------
package body polygons_2d with SPARK_Mode is

  function edges_of_polygon(polygon: polygon_2d; i: nat) return segment_2d is
    result: segment_2d  :=
      (p1 => polygon.vertices(i),
       p2 => polygon.vertices(next_index(polygon, i)));
  begin
    pragma Assert(result = weak_edges_of_polygon(polygon, i));
    pragma Assert(polygon_2d_constraint(polygon));
    -- A subset of polygon_2d_constraint:
    pragma Assert(for all i2 in 0 .. polygon.num_vertices-1 =>
                    (for all j in 0 .. polygon.num_vertices-1 =>
                       (can_subtract(polygon.vertices(j), polygon.vertices(i2)) and then
                        can_add(polygon.vertices(i2),
                          vector_from_point_to_point(polygon.vertices(i2),
                            polygon.vertices(j))) and then
                        segments_comparable(weak_edges_of_polygon(polygon, i2),
                          weak_edges_of_polygon(polygon, j)))
                    )
                 );
    pragma Assert(for all i2 in 0 .. polygon.num_vertices-1 =>
                    (for all j in 0 .. polygon.num_vertices-1 =>
                       segments_comparable(weak_edges_of_polygon(polygon, i2),
                         weak_edges_of_polygon(polygon, j))
                    )
                 );
    pragma Assert(for all i2 in 0 .. polygon.num_vertices-1 =>
                    (for all j in i .. i =>
                       segments_comparable(weak_edges_of_polygon(polygon, i2),
                         weak_edges_of_polygon(polygon, j))
                    )
                 );
    pragma Assert(for all i2 in 0 .. polygon.num_vertices-1 =>
                    segments_comparable(weak_edges_of_polygon(polygon, i2),
                      weak_edges_of_polygon(polygon, i))
                 );
    pragma Assert(for all i2 in 0 .. polygon.num_vertices-1 =>
                    segments_comparable(weak_edges_of_polygon(polygon, i2),
                      result));
    pragma Assert(for all i2 in i .. i =>
                    (for all j in 0 .. polygon.num_vertices-1 =>
                       segments_comparable(weak_edges_of_polygon(polygon, i2),
                         weak_edges_of_polygon(polygon, j))
                    )
                 );
    pragma Assert(for all j in 0 .. polygon.num_vertices-1 =>
                    segments_comparable(weak_edges_of_polygon(polygon, i),
                      weak_edges_of_polygon(polygon, j))
                 );
    pragma Assert(for all i2 in 0 .. polygon.num_vertices-1 =>
                    segments_comparable(result, weak_edges_of_polygon(polygon, i2)));
    pragma Assert(for all i2 in 0 .. polygon.num_vertices-1 =>
                    segments_comparable(weak_edges_of_polygon(polygon, i2), result));
    --  Post condition:
    --  edges_of_polygon'Result = (p1 => polygon.vertices(i),
    --                                       p2 => polygon.vertices(next_index(polygon, i)))
    --    and (for all i2 in 0 .. polygon.num_vertices-1 =>
    --           segments_comparable(edges_of_polygon'Result, weak_edges_of_polygon(polygon, i2))
    --         and segments_comparable(weak_edges_of_polygon(polygon, i2), edges_of_polygon'Result));
    return result;
  end edges_of_polygon;

  procedure Lemma_Edges_Of_Polygon_Same_When_Weak(p: polygon_2d; i:nat) is null;

   -- From polygons_2d.pvs:
   --   % idx being of type below(p`num_vertices) implicitly requires p to have
   --   % a non-zero number of vertices
   --   find_helper(p: polygon_2d, idx: below(p`num_vertices), point: point_2d):
   --       RECURSIVE {i: int | i >= -1 AND i < p`num_vertices} =
   --     % Terminating condition when found
   --     IF p`vertices(idx) = point THEN
   --       idx
   --     % Terminating condition when not found
   --     ELSIF idx = p`num_vertices - 1 THEN
   --       -1
   --     ELSE
   --     % Look at the next point
   --       find_helper(p, idx + 1, point)
   --     ENDIF
   --     MEASURE p`num_vertices - 1 - idx;
   --   % Find index value of vertex matching point or return -1 if there's no match
   --   find_index(polygon: polygon_2d, point: point_2d):
   --     {i: int | (i = -1 AND NOT
   --                  EXISTS(v: (vertex?(polygon))): v = point) OR
   --               (i >= 0 AND i < polygon`num_vertices AND
   --                 polygon`vertices(i) = point)} =
   --     IF polygon`num_vertices = 0 THEN
   --       -1
   --     ELSE
   --       find_helper(polygon, 0, point)
   --     ENDIF;
   function find_index(polygon: polygon_2d; point: point_2d) return integer is
      result: integer := -1;
   begin
      for idx in 0 .. polygon.num_vertices-1 loop
         if polygon.vertices(idx) = point then
            result := idx;
            exit; -- break out of loop
         end if;
         pragma Loop_Invariant(result = -1 and not
                                 (for some i in 0 .. idx =>
                                    polygon.vertices(i) = point));
      end loop;
      return result;
  end find_index;

  --  polygon_edges_do_not_cross?(p: polygon_2d): bool =
  --    FORALL (i, j: below(p`num_vertices)):
  --      LET ei = edges_of_polygon(p)(i), ej = edges_of_polygon(p)(j) IN
  --        FORALL (q: (is_point_on_segment?(ei))):
  --          is_point_on_segment?(ej)(q) IMPLIES
  --            i = j OR q = ei`p1 OR q = ei`p2
  --...
  --  polygon_edges_do_not_cross?_alt(p: polygon_2d): bool =
  --  FORALL(i: below(p`num_vertices),
  --         j: {n: below(p`num_vertices) | n /= i}):
  --    LET ei = edges_of_polygon(p)(i), ej = edges_of_polygon(p)(j) IN
  --    IF equal_or_adjacent_edge?(p`num_vertices, i, j) THEN
  --      NOT ^(ei) = -^(ej)
  --    ELSE
  --      NOT are_segments_intersecting?(ei)(ej)
  --    ENDIF;
  --...
  --PROVEN LEMMA:
  --  polygon_edges_do_not_cross_defs_eq: LEMMA
  --    FORALL(p: polygon_2d):
  --      polygon_edges_do_not_cross?(p) IFF
  --        polygon_edges_do_not_cross?_alt(p)
  function polygon_edges_do_not_cross(p: polygon_2d) return Boolean is
    result: Boolean := true; -- True until determined false
  begin
    pragma Assert(polygon_2d_constraint(p));
    pragma Assert(for all i2 in 0 .. p.num_vertices-1 =>
                        (for all j2 in 0 .. p.num_vertices-1 =>
			segments_comparable(weak_edges_of_polygon (p,i2), weak_edges_of_polygon(p,j2))));
    for i in 0 .. p.num_vertices-1 loop
      for j in 0 .. p.num_vertices-1 loop
        pragma Assert(cond_pre(p));
        if (not cond(p, i, j)) then
          return false;
        end if;
        pragma Loop_Invariant(polygon_2d_constraint(p));
        -- Corresponds to post condition
        pragma Loop_Invariant((for all i2 in 0 .. i =>
                                 (for all j2 in 0 .. j =>
                                    cond(p, i2, j2))));
      end loop;
      pragma Loop_Invariant(polygon_2d_constraint(p));
      -- Corresponds to post condition
      pragma Loop_Invariant(result =
                            (for all i2 in 0 .. i =>
                               (for all j2 in 0 .. p.num_vertices - 1 =>
                                  cond(p, i2, j2))));
    end loop;
    -- Post condition
    pragma Assert(result =
      (for all i in 0 .. p.num_vertices-1 =>
         (for all j in 0 .. p.num_vertices-1 =>
            cond(p, i, j))));
    return result;
  end polygon_edges_do_not_cross;

  --  is_point_in_polygon_recursive?(polygon: polygon_2d, point: point_2d,
  --                                index: below(polygon`num_vertices),
  --                                is_inside: bool): RECURSIVE bool =
  --    LET segment: segment_2d = edges_of_polygon(polygon)(index) IN
  --    IF (index = polygon`num_vertices - 1) THEN
  --      is_inside XOR is_right_point_ray_intersection?(point, segment)
  --    ELSE
  --      is_point_in_polygon_recursive?(polygon, point, index + 1,
  --              is_inside XOR is_right_point_ray_intersection?(point, segment))
  --    ENDIF
  --   MEASURE (polygon`num_vertices - index);
  --...
  --  % Is the point inside the polygon, but not on the edge of the polygon?
  --  is_point_in_polygon_exclusive?(polygon: polygon_2d)(point: point_2d): bool =
  --    IF is_point_on_polygon_perimeter?(polygon)(point) THEN
  --      FALSE
  --    ELSE
  --      is_point_in_polygon_recursive?(polygon, point, 0, FALSE)
  --    ENDIF;
  --NOTE: Changing recursive function into iteration
  function is_point_in_polygon_exclusive(polygon: polygon_2d; point: point_2d) return Boolean is
    is_inside: Boolean;
    segment: segment_2d;
  begin
    if is_point_on_polygon_perimeter(polygon, point) then
      is_inside := False;
    else
      is_inside := False;
      --  is_point_in_polygon_recursive?(polygon, point, 0, FALSE)
      for index in 0 .. polygon.num_vertices-1 loop
        segment := edges_of_polygon(polygon, index);
        is_inside := is_inside xor is_right_point_ray_intersection(point, segment);
      end loop;
    end if;
    return is_inside;
  end is_point_in_polygon_exclusive;

end polygons_2d;
