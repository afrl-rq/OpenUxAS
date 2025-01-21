with polygon_attributes;
with polygon_merge;
with injection_props;
with vertex_injection;

package body buildVisibilityGraph with SPARK_Mode is
   use polygon_attributes;
   use polygon_merge;
   use injection_props;
   use polygons_2d;
   use vertex_injection;

   function strip_weak(fsw: FSFW.FS.Finseq) return FSFS.FS.Finseq is
      result: FSFS.FS.Finseq := FSFS.FS.Empty_Seq;
      first: weak_segment_2d;
   begin
      for idx in 0 .. fsw.Length - 1 loop
         first := fsw.Seq(idx);
         if is_valid_segment(first) then
            result := FSFS.Prepend(to_segment_2d(first), result);
         end if;
      end loop;

      return result;
   end strip_weak;

   function mergePolygonWithAllMergeablePolygons(p: zone_polygon; other_polys: FSFZ.FS.Finseq) 
                                                 return Merge_Result is
      Polygon: zone_polygon := p;
      Other_Polygons: FSFZ.FS.Finseq := other_polys;
      idx: Natural := 0;
   begin
      -- Iterate through Other_Polygons
      while idx < Other_Polygons.Length loop
         pragma Loop_Variant(Decreases => Other_Polygons.Length - idx);

         declare
            Current_Polygon: zone_polygon := Other_Polygons.Seq(idx);
         begin
            -- Check if the polygons can be merged
            if Polygon.is_keep_in = Current_Polygon.is_keep_in and then
               merge_pre_condition(Polygon.polygon, Current_Polygon.polygon) then
               -- Perform the merge
               declare
                  P_Ccw, Current_Ccw: zone_polygon;
               begin
                  if ccw_vertex_order(Polygon.polygon) then
                     P_Ccw := Polygon;
                  else
                     P_Ccw := reverse_polygon(Polygon);
                  end if;

                  if ccw_vertex_order(Current_Polygon.polygon) then
                     Current_Ccw := Current_Polygon;
                  else
                     Current_Ccw := reverse_polygon(Current_Polygon);
                  end if;

                  Polygon := (
                               eps => Polygon.eps,
                               polygon => merge_seq(P_Ccw.polygon, Current_Ccw.polygon),
                               is_keep_in => Polygon.is_keep_in,
                               is_original => Polygon.is_original
                              );

                  -- Remove the current polygon from Other_Polygons
                  Other_Polygons := FSFZ.Remove(Other_Polygons, idx);
               end;
            else
               -- Increment idx if no merge occurs
               idx := idx + 1;
            end if;
         end;
      end loop;

      -- Return the result
      return (Length => Other_Polygons.Length, Polygon => Polygon, Other_Polygons => Other_Polygons);
   end mergePolygonWithAllMergeablePolygons;

   function mergeMergeablePolygons(allPolygons: FSFZ.FS.Finseq) return FSFZ.FS.Finseq is
      -- Initialize the remaining polygons with the input
      remainingPolygons: FSFZ.FS.Finseq := allPolygons;
      -- Initialize the result as an empty sequence
      mergedPolygons: FSFZ.FS.Finseq := FSFZ.FS.Empty_Seq;
   begin
      -- While there are polygons left to merge
      while remainingPolygons.Length > 0 loop
         pragma Loop_Variant(Decreases => remainingPolygons.Length);

         declare
            -- Extract the first polygon from the remaining polygons
            p: zone_polygon := remainingPolygons.Seq(0);
            -- Merge the first polygon with all mergeable polygons in the rest
            MergeResult: Merge_Result := mergePolygonWithAllMergeablePolygons(p, FSFZ.Remove(remainingPolygons, 0));
         begin
            -- Add the merged polygon to the result
            mergedPolygons := FSFZ.Append(mergedPolygons, MergeResult.Polygon);

            -- Update the remaining polygons
            remainingPolygons := MergeResult.Other_Polygons;
         end;
      end loop;

      return mergedPolygons;
   end mergeMergeablePolygons;

   function isEdgeGood(polygons: FSFZ.FS.Finseq; edge: segment_2d) return Boolean is
      Midpoint: point_2d := segment_midpoint(edge);
      Found_Keep_In: Boolean := False;
      Found_Keep_Out: Boolean := False;
      Good_Edge: Boolean := True; -- Default to True, updated based on checks
   begin
      -- Check if the edge does not intersect or overlap any polygon edge improperly
      if intersectionFound(polygons, edge) then
         Good_Edge := False;
      end if;

      -- If still considered "good," check for at least one "keep-in" zone
      if Good_Edge then
         for i in 0 .. polygons.Length - 1 loop
            declare
               zp: zone_polygon := polygons.Seq(i);
            begin
               if zp.is_keep_in and is_point_in_polygon_exclusive(zp.polygon, Midpoint) then
                  Found_Keep_In := True;
                  exit; -- No need to check further
               end if;
            end;
         end loop;

         if not Found_Keep_In then
            Good_Edge := False;
         end if;
      end if;

      -- If still considered "good," check for any "keep-out" zone
      if Good_Edge then
         for i in 0 .. polygons.Length - 1 loop
            declare
               zp: zone_polygon := polygons.Seq(i);
            begin
               if not zp.is_keep_in and is_point_in_polygon_exclusive(zp.polygon, Midpoint) then
                  Found_Keep_Out := True;
                  exit; -- No need to check further
               end if;
            end;
         end loop;

         if Found_Keep_Out then
            Good_Edge := False;
         end if;
      end if;

      -- Return the final result
      return Good_Edge;
   end isEdgeGood;

   function goodEdgePortions(edgePortions: FSFS.FS.Finseq; Other_Polygons: FSFZ.FS.Finseq) return FSFS.FS.Finseq is
      Result: FSFS.FS.Finseq := FSFS.FS.Empty_Seq;
   begin
      -- Iterate through all segments in edgePortions
      for i in 0 .. edgePortions.Length - 1 loop
         declare
            EdgePortion: segment_2d := edgePortions.Seq(i);
         begin
            -- Check if the edge portion is "good"
            if isEdgeGood(Other_Polygons, EdgePortion) then
               -- Add the good edge portion to the result
               Result := FSFS.Append(Result, EdgePortion);
            end if;
         end;
      end loop;

      return Result;
   end goodEdgePortions;

   -- Finds all intersections of edge with other polygons
   function portionEdge(edge: segment_2d; Other_Polygons: FSFZ.FS.Finseq) return uniq_vertex_list is
      Result: uniq_vertex_list := empty_seq;
   begin
      -- Iterate through all polygons in Other_Polygons
      for i in 0 .. Other_Polygons.Length - 1 loop
         declare
            Current_Polygon: zone_polygon := Other_Polygons.Seq(i);
            Intersections: bounded_vertex_list := injected_edge(edge, Current_Polygon.polygon);
         begin
            -- Add intersections to Result, ensuring no duplicates
            for j in 0 .. Intersections.num_vertices - 1 loop
               declare
                  Point: point_2d := Intersections.vertices(j);
               begin
                  if not in_vertex_seq(Result, Point) then
                     Result := append_to_uniq_vertex_seq(Result, Point);
                  end if;
               end;
            end loop;
         end;
      end loop;

      return Result;
   end portionEdge;

   -- Constructs edge portions from edge points, ensuring no duplicates
   function buildEdgePortions(edge: segment_2d; edgePts: uniq_vertex_list) return FSFS.FS.Finseq is
      Result: FSFS.FS.Finseq := FSFS.FS.Empty_Seq;
      IsValidSegment: Boolean;
   begin
      -- Iterate through all pairs of points in edgePts
      for i in 0 .. edgePts.num_vertices - 1 loop
         for j in i + 1 .. edgePts.num_vertices - 1 loop
            declare
               P1: point_2d := edgePts.vertices(i);
               P2: point_2d := edgePts.vertices(j);
               Candidate: segment_2d := (P1 => P1, P2 => P2);
            begin
               -- Check if both points lie on the segment
               if is_point_on_segment(P1, edge) and is_point_on_segment(P2, edge) then
                  -- Verify the conditions for the candidate segment
                  IsValidSegment := True;

                  for k in 0 .. edgePts.num_vertices - 1 loop
                     declare
                        P: point_2d := edgePts.vertices(k);
                     begin
                        if P /= P1 and P /= P2 and is_point_on_segment(P, Candidate) then
                           IsValidSegment := False;
                           exit; -- Stop further checks for this segment
                        end if;
                     end;
                  end loop;

                  -- Add the segment if it is valid and not a duplicate
                  if IsValidSegment and not FSFS.Is_In(Result, Candidate) then
                     Result := FSFS.Append(Result, Candidate);
                  end if;
               end if;
            end;
         end loop;
      end loop;
      
      return Result;
   end buildEdgePortions;

   function addGoodEdgesFromEdges(edges: FSFS.FS.Finseq; Other_Polygons: FSFZ.FS.Finseq) return FSFS.FS.Finseq is
      Result: FSFS.FS.Finseq := FSFS.FS.Empty_Seq;
   begin
      -- Iterate through all edges
      for i in 0 .. edges.Length - 1 loop
         declare
            Edge: segment_2d := edges.Seq(i);
            GoodEdge: Boolean := isEdgeGood(Other_Polygons, Edge);
         begin
            if GoodEdge then
               -- Add the edge directly to the result if it's good
               Result := FSFS.Append(Result, Edge);
            else
               -- Process edge portions if the whole edge isn't good
               declare
                  EdgePoints: uniq_vertex_list := append_to_uniq_vertex_seq(portionEdge(Edge, Other_Polygons), Edge.P2);
                  EdgePortions: FSFS.FS.Finseq := buildEdgePortions(Edge, EdgePoints);
                  GoodPortions: FSFS.FS.Finseq := goodEdgePortions(EdgePortions, Other_Polygons);
               begin
                  -- Add the good edge portions to the result
                  Result := FSFS.FS.Concat(Result, GoodPortions);
               end;
            end if;
         end;
      end loop;

      return Result;
   end addGoodEdgesFromEdges;

   function edges_of_polygon(p: zone_polygon) return FSFS.FS.Finseq is
      Result: FSFS.FS.Finseq := FSFS.FS.Empty_Seq;
   begin
      for i in 0 .. p.polygon.num_vertices loop
         Result := FSFS.Append(Result, edges_of_polygon(p.polygon, i));
      end loop;
      return Result;
   end edges_of_polygon;

   function addGoodEdges(mergedPolygons: FSFZ.FS.Finseq) return FSFS.FS.Finseq is
      Result: FSFS.FS.Finseq := FSFS.FS.Empty_Seq;
   begin
      -- Iterate through all polygons in mergedPolygons
      for idx in 0 .. mergedPolygons.Length - 1 loop
         declare
            P: zone_polygon := mergedPolygons.Seq(idx);
            Other_Polygons: FSFZ.FS.Finseq := FSFZ.Remove(mergedPolygons, idx);
            GoodEdgesFromPolygon: FSFS.FS.Finseq := addGoodEdgesFromPolygon(P, Other_Polygons);
         begin
            -- Append the edges from the current polygon to the result
            Result := FSFS.FS.Concat(Result, GoodEdgesFromPolygon);
         end;
      end loop;

      return Result;
   end addGoodEdges;

   function findGoodConnections(edge: segment_2d; allPolygons: FSFZ.FS.Finseq; remainingEdges: FSFS.FS.Finseq) return FSFS.FS.Finseq is
      Result: FSFS.FS.Finseq := FSFS.FS.Empty_Seq;
   begin
      -- Iterate through all remaining edges
      for i in 0 .. remainingEdges.Length - 1 loop
         declare
            Other: segment_2d := remainingEdges.Seq(i);
            Potentials: FSFW.FS.Finseq := FSFW.FS.Empty_Seq;
            GoodPortions: FSFS.FS.Finseq(Length => 4);
         begin
            Potentials := FSFW.Append(Potentials, (P1 => edge.P1, P2 => Other.P1));
            Potentials := FSFW.Append(Potentials, (P1 => edge.P1, P2 => Other.P2));
            Potentials := FSFW.Append(Potentials, (P1 => edge.P2, P2 => Other.P1));
            Potentials := FSFW.Append(Potentials, (P1 => edge.P2, P2 => Other.P2));
            GoodPortions := goodEdgePortions(strip_weak(Potentials), allPolygons);
            -- Append the good portions to the result
            Result := FSFS.FS.Concat(Result, GoodPortions);
         end;
      end loop;

      return Result;
   end findGoodConnections;

   function includeGoodConnections(allPolygons: FSFZ.FS.Finseq; perimeterEdges: FSFS.FS.Finseq) return FSFS.FS.Finseq is
      Result: FSFS.FS.Finseq := FSFS.FS.Empty_Seq;
   begin
      -- Iterate through the perimeter edges
      for idx in 0 .. perimeterEdges.Length - 1 loop
         declare
            Edge: segment_2d := perimeterEdges.Seq(idx);

            -- Remaining edges are all edges after the current one
            RemainingEdges: FSFS.FS.Finseq := FSFS.FS.Slice(perimeterEdges, idx + 1, perimeterEdges.Length - 1);

            -- Find good connections for the current edge
            GoodConnections: FSFS.FS.Finseq := findGoodConnections(Edge, allPolygons, RemainingEdges);
         begin
            -- Append the good connections to the result
            Result := FSFS.FS.Concat(Result, GoodConnections);
         end;
      end loop;

      return Result;
   end includeGoodConnections;

end buildVisibilityGraph;
