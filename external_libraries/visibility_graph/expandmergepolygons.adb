with perimeter_props;

package body expandMergePolygons with SPARK_Mode is
   use perimeter_props;

   function eliminate_redundant_vertices(epsilon : eps_type; polygon : zone_polygon)
                                         return zone_polygon is
      Poly : polygon_2d := polygon.polygon;
      F : uniq_vertex_list := eliminate_redundant_vertices(epsilon, Poly);
      Cand : polygon_2d := (Num_Vertices => F.num_vertices, Vertices => F.vertices);
      Is_Eps_Poly : Boolean := (F.num_vertices >= 3 and is_eps_polygon(Cand, polygon.eps));
      new_poly : polygon_2d := (if Is_Eps_Poly then Cand else Poly);
   begin
      return (eps => polygon.eps,
              polygon => new_poly,
              is_keep_in => polygon.is_keep_in,
              is_original => not Is_Eps_Poly);
   end eliminate_redundant_vertices;

   --PVS:
   --  remove_very_small_polys_helper(polygonList: fs_zone_polygons,
   --                                 index: upto(polygonList`length),
   --                                 epsilon: eps_type): RECURSIVE
   --      {f: fs_zone_polygons | f`length <= polygonList`length - index AND
   --                             (length(f) = 0 OR eps(f) = eps(polygonList))} =
   --    IF index = polygonList`length THEN
   --      empty_seq
   --    ELSIF area(polygonList`seq(index)`polygon) > epsilon THEN
   --      prepend(polygonList`seq(index),
   --              remove_very_small_polys_helper(polygonList, index+1, epsilon))
   --    ELSE
   --      remove_very_small_polys_helper(polygonList, index+1, epsilon)
   --    ENDIF
   --    MEASURE polygonList`length - index;
   --
   --  remove_very_small_polys(polygonList: finseq[simple_polygon_2d],
   --                          epsilon: eps_type):
   --      {f: finseq[simple_polygon_2d] | f`length <= polygonList`length} =
   --    remove_very_small_polys_helper(polygonList, 0, epsilon);
   function remove_very_small_polys(polygonList: FSFZ.FS.Finseq; epsilon : eps_type)
                                    return FSFZ.FS.Finseq
   is
      Result: FSFZ.FS.Finseq := FSFZ.FS.Empty_Seq;
   begin
      for index in 0 .. polygonList.length loop
         if area(polygonList.seq(index).polygon) > epsilon then
            Result := FSFZ.Append(Result, polygonList.Seq(index));
         end if;
      end loop;
      return Result;
   end remove_very_small_polys;

end expandMergePolygons;
