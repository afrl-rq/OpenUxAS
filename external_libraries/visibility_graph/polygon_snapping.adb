package body polygon_snapping with SPARK_Mode is

   function snap_to_boundary_of(p : point_2d; environment_temp : polygon_2d;
                                eps : eps_type) return point_2d
   is
      pointemp : constant point_2d := projection_onto_boundary_of(p, environment_temp);
   begin
      if distance(p, pointemp) <= eps then
         return pointemp;
      else
         return p;
      end if;
   end snap_to_boundary_of;

   function snapped_vertex_list(epsilon : eps_type; A, B : simple_polygon_2d)
                                return uniq_vertex_list is
      Result : uniq_vertex_list := empty_seq;
   begin
      for i in 0 .. A.num_vertices - 1 loop
         if boundary_distance(A.vertices(i), B) < epsilon then
            Result := append_to_uniq_vertex_seq(Result, snap_to_boundary_of(A.vertices(i), B, epsilon));
         else
            Result := append_to_uniq_vertex_seq(Result, A.vertices(i));
         end if;
         pragma Loop_Invariant(Result.num_vertices = i + 1);
      end loop;
      return Result;
   end snapped_vertex_list;

   function vtx_to_edge_distances(A: Simple_Polygon_2D) return FSFF.FS.Finseq is
      Result : FSFF.FS.Finseq := FSFF.FS.Empty_Seq;
   begin
      -- Iterate over all vertex pairs (i, j) in the polygon
      pragma Assert(A.num_vertices > 2);
      for i in 0 .. A.Num_Vertices - 1 loop
         pragma Loop_Invariant(Result.Length <= i * (A.num_vertices - 2));
         for j in 0 .. A.Num_Vertices - 1 loop
            pragma Loop_Invariant(Result.Length <= i * (A.num_vertices - 2) + j);
            -- Skip invalid combinations where i = j or i is the next vertex of j
            if i /= j and i /= next_index(A, j) then
               Result := FSFF.Append(Result, distance(A.Vertices(i), edges_of_polygon(A, j)));
            end if;
         end loop;
      end loop;
      pragma Assert(Result.Length <= A.num_vertices * (A.num_vertices - 2));

      return Result;
   end vtx_to_edge_distances;

   function snap_margin(A: simple_polygon_2d) return nn_float is
      Result: nn_float := Float'Last;
      Distances: FSFF.FS.Finseq := vtx_to_edge_distances(A);
   begin
      for Val of Distances.Seq loop
         if Val < Result then
            Result := Val;
         end if;
      end loop;
      return Result;
   end snap_margin;

   function snap_near_inner_fn(epsilon: eps_type; polygonList: FSFP.FS.Finseq;
                               idx1: Natural; idx2: Natural) return FSFP.FS.Finseq is
      Len: constant Natural := polygonList.Length;
      ModifyingList: constant FSFP.FS.Finseq := polygonList;
      Result: FSFP.FS.Finseq := FSFP.FS.Empty_Seq;
   begin
      -- Check distance condition
      if distance(polygonList.Seq(idx1), polygonList.Seq(idx2)) > 0.0 and then
        distance(polygonList.Seq(idx1), polygonList.Seq(idx2)) < epsilon then
         for i in 0 .. Len - 1 loop
            if i = idx1 then
               declare
                  poly1: constant simple_polygon_2d := ModifyingList.Seq(idx1);
               begin
                  if is_snapping_eligible(epsilon, poly1) then
                     Result := FSFP.Append(Result, snap_vertices_to_polygon(epsilon, poly1, ModifyingList.Seq(idx2)));
                  else
                     Result := FSFP.Append(Result, poly1);
                  end if;
               end;
            elsif i = idx2 then
               declare
                  poly2: constant simple_polygon_2d := ModifyingList.Seq(idx2);
               begin
                  if is_snapping_eligible(epsilon, poly2) then
                     Result := FSFP.Append(Result, snap_vertices_to_polygon(epsilon, poly2, ModifyingList.Seq(idx1)));
                  else
                     Result := FSFP.Append(Result, poly2);
                  end if;
               end;
            else
               Result := FSFP.Append(Result, ModifyingList.Seq(i));
            end if;
         end loop;

      else
         Result := ModifyingList;
      end if;

      return Result;
   end snap_near_inner_fn;

   function snap_near_inner_loop(epsilon: eps_type; polygonList: FSFP.FS.Finseq;
                                 idx1, n: Natural) return FSFP.FS.Finseq is
      Len: constant Natural := polygonList.Length;
      Result: FSFP.FS.Finseq := polygonList;
   begin
      -- Iterate for `n` steps, incrementing each time
      for Step in 1 .. n loop
         declare
            idx2: constant Natural := Len - Step;
         begin
            -- Apply the snapping function and update Result
            Result := snap_near_inner_fn(epsilon, Result, idx1, idx2);
         end;
      end loop;

      return Result;
   end snap_near_inner_loop;

   function snap_near_outer_loop(epsilon: eps_type; polygonList: FSFP.FS.Finseq;
                                 idx1: Natural) return FSFP.FS.Finseq is
      Len: constant Natural := polygonList.Length;
      Result: FSFP.FS.Finseq := polygonList;
   begin
      -- Iterate from idx1 to Len - 1
      for Outer_Idx in idx1 .. Len - 1 loop
         declare
            Snap_Inner: constant FSFP.FS.Finseq :=
              snap_near_inner_loop(epsilon, Result, Outer_Idx, Len - Outer_Idx - 1);
         begin
            Result := Snap_Inner;
         end;
      end loop;

      return Result;
   end snap_near_outer_loop;

end polygon_snapping;
