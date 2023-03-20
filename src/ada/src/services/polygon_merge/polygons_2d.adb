-- -----------------------------------------------------------------------------
-- polygons_2d.adb              Dependable Computing
-- Corresponds to logic from polygons_2d.pvs
-- -----------------------------------------------------------------------------
package body polygons_2d with SPARK_Mode is

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
      for index in 1 .. polygon.num_vertices-1 loop
        segment := edges_of_polygon(polygon, index);
        is_inside := is_inside xor is_right_point_ray_intersection(point, segment);
      end loop;
    end if;
    return is_inside;
  end is_point_in_polygon_exclusive;

end polygons_2d;
