
package body findAllVisibleEdges with SPARK_Mode is

   function segmentIntersectsPolygon(edge: segment_2d; polygon: polygon_2d) return Boolean is
      polygonEdge: segment_2d;
   begin
      for idx in 0 .. polygon.num_vertices - 1 loop
         polygonEdge := edges_of_polygon(polygon, idx);

         -- Check if segments are intersecting
         if are_segments_intersecting(edge, polygonEdge) and then
           not (is_t_type_intersection(edge, polygonEdge) or
                    is_segment_extending(edge, polygonEdge)) then
            return True;
         end if;
      end loop;

      return False;
   end segmentIntersectsPolygon;

   function intersectionFound(allPolygons: FSFZ.FS.Finseq; edge: segment_2d) return Boolean is
   begin
      for idx in 0 .. allPolygons.length - 1 loop
         if segmentIntersectsPolygon(edge, allPolygons.seq(idx).polygon) then
            return True;
         end if;
      end loop;
      return False;
   end intersectionFound;

   function findVisibleEdges(polygonThis, polygonThat: zone_polygon;
                             allPolygons: FSFZ.FS.Finseq) return FSFS.FS.Finseq is
      result: FSFS.FS.Finseq := FSFS.FS.Empty_Seq;
      v1, v2, centerPt: point_2d;
      goodEdge: Boolean;
      edgeNew: segment_2d;
   begin
      -- Iterate over vertices of polygonThis
      for idx1 in 0 .. polygonThis.polygon.num_vertices - 1 loop
         v1 := polygonThis.polygon.vertices(idx1);

         -- Iterate over vertices of polygonThat
         for idx2 in 0 .. polygonThat.polygon.num_vertices - 1 loop
            v2 := polygonThat.polygon.vertices(idx2);
            centerPt := 0.5 * (v1 + v2);

            -- Determine if the edge is a good edge
            goodEdge := (not polygonThis.is_keep_in) or else
              is_point_in_polygon_inclusive(polygonThis.polygon, centerPt);

            if v1 /= v2 and goodEdge then
               edgeNew := (p1 => v1, p2 => v2);

               -- Check for intersection with all polygons
               if not intersectionFound(allPolygons, edgeNew) then
                  result := FSFS.Prepend(edgeNew, result);
               end if;
            end if;
         end loop;
      end loop;
      return result;
   end findVisibleEdges;

   function findAllVisibleEdges(p: zone_polygon; mergedPolygons: FSFZ.FS.Finseq) return FSFS.FS.Finseq is
      result: FSFS.FS.Finseq := FSFS.FS.Empty_Seq;
      that: zone_polygon;
   begin
      for idx in 0 .. mergedPolygons.length - 1 loop
         that := mergedPolygons.seq(idx);
         result := FSFS.FS.Concat(findVisibleEdges(p, that, mergedPolygons), result);
      end loop;

      return result;
   end findAllVisibleEdges;

end findAllVisibleEdges;
