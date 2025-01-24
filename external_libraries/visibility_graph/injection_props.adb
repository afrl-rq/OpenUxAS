-- -----------------------------------------------------------------------------
-- injection_props.adb              Dependable Computing
-- Corresponds to logic from injection_props.pvs
-- -----------------------------------------------------------------------------
with segments_2d;

package body injection_props with SPARK_Mode is
   use segments_2d;

   function merge_pre_condition(A, B : simple_polygon_2d) return Boolean is
      result: Boolean := False;
   begin
      -- Check if any vertex of A is inside B
      vertex_A_loop: for I in 0 .. A.num_vertices - 1 loop
         if is_point_in_polygon_inclusive(B, A.vertices(I)) then
            result := True;
            exit vertex_A_loop;
         end if;
      end loop vertex_A_loop;

      -- Check if any vertex of B is inside A
      vertex_B_loop: for J in 0 .. B.num_vertices - 1 loop
         if is_point_in_polygon_inclusive(A, B.vertices(J)) then
            result := True;
            exit vertex_B_loop;
         end if;
      end loop vertex_B_loop;

      -- Check if any edge of A intersects any edge of B
      intesect_loop: for I in 0 .. A.num_vertices - 1 loop
         for J in 0 .. B.num_vertices - 1 loop
            declare
               Edge_A : segment_2d := (p1 => A.vertices(I), p2 => A.vertices((I + 1) mod A.num_vertices));
               Edge_B : segment_2d := (p1 => B.vertices(J), p2 => B.vertices((J + 1) mod B.num_vertices));
            begin
               if are_segments_intersecting(Edge_A, Edge_B) then
                  result := True;
                  exit intesect_loop;
               end if;
            end;
         end loop;
      end loop intesect_loop;

      -- None of the conditions are satisfied
      return result;
   end merge_pre_condition;

end injection_props;
