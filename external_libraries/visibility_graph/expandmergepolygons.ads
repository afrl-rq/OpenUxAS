-- -----------------------------------------------------------------------------
-- expandMergePolygons.ads              Dependable Computing
-- Corresponds to logic from expandMergePolygons.pvs
-- -----------------------------------------------------------------------------
with eps_polygons;
with eps_segments;
with polygons_2d;
with polygon_snapping;
with vertex_list;
with zone_polygons;

package expandMergePolygons with SPARK_Mode is

   use eps_polygons;
   use eps_segments;
   use polygons_2d;
   use polygon_snapping;
   use vertex_list;
   use zone_polygons;

   -- PVS:
   --   eliminate_redundant_vertices(epsilon: eps_type,
   --                                polygon: zone_polygon): zone_polygon =
   --     LET poly: eps_polygon(polygon`eps) = polygon`polygon,
   --         s: finseq[point] = uniq_vertex_list_to_seq(poly`num_vertices,
   --                                             poly`vertices),
   --         % The call to eliminate_redundant_vertices uses the passed in
   --         % epsilon and *not* the epsilon associated with the polygon. See
   --         % line 105 in the old VisibilityGraph.cpp file.
   --         f: finseq[point] = eliminate_redundant_vertices(epsilon, s),
   --         cand = (# num_vertices := f`length, vertices := f`seq #),
   --         is_eps_poly = f`length >= 3 AND eps_polygon?(polygon`eps)(cand),
   --         new: eps_polygon(polygon`eps) = IF is_eps_poly THEN cand ELSE poly ENDIF
   --     IN
   --       (# eps := polygon`eps,   %%% epsilon
   --          polygon := new,
   --          keep_in? := polygon`keep_in?,
   --          original? := NOT is_eps_poly #)
   function eliminate_redundant_vertices(epsilon : eps_type; polygon : zone_polygon)
                                         return zone_polygon
     with
       Post =>
         eliminate_redundant_vertices'Result.eps = polygon.eps and then
         eliminate_redundant_vertices'Result.is_keep_in = polygon.is_keep_in and then
         (if eliminate_redundant_vertices'Result.is_original = False then
            is_eps_polygon(eliminate_redundant_vertices'Result.polygon, polygon.eps) and
              eliminate_redundant_vertices'Result.polygon.num_vertices >= 3
                else
                  eliminate_redundant_vertices'Result.polygon = polygon.polygon);

   package FSFP renames zone_polygons.FSFP;

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
   --  remove_very_small_polys(polygonList: fs_zone_polygons,
   --                          epsilon: eps_type):
   --      {f: fs_zone_polygons | f`length <= polygonList`length} =
   --    remove_very_small_polys_helper(polygonList, 0, epsilon);
   function remove_very_small_polys(polygonList: FSFZ.FS.Finseq; epsilon: eps_type) return FSFZ.FS.Finseq;

   --PVS:
   --  % Corresponds to visilibity.cpp function
   --  %   bool Polygon::boost_union(vector<Polygon>, vector<Polygon>, double)
   --  boost_union(polygonList: fs_zone_polygons, epsilon:eps_type):
   --      {f: fs_zone_polygons | f`length <= polygonList`length AND
   --         (length(f) = 0 OR eps(f) = eps(polygonList))} =
   --    IF polygonList`length = 0 THEN
   --      empty_seq
   --    ELSE
   --      LET eps = eps(polygonList) IN
   --      % Logic from find_overlap is now being done by mergeMergeablePolygons
   --      % in the buildVisibilityGraph theory.
   --      eps_polygon_seq_to_zone_polygon_seq(eps,
   --        snap_near(eps, polygonList),
   --        polygonList`seq(0))
   --    ENDIF;
   function boost_union(polygonList: FSFZ.FS.Finseq) return FSFZ.FS.Finseq is
     (if polygonList.length = 0 then
         FSFZ.FS.Empty_Seq
      else
         eps_polygon_seq_to_zone_polygon_seq(eps(polygonList),
        snap_near(eps(polygonList), zone_polygon_seq_to_polygon_seq(polygonList)),
        polygonList.seq(0)))
     with
       Pre => polygonList.length > 0;

  --PVS:
  --  % Corresponds to visilibity.cpp function
  --  %   bool Polygon::offset_polygons(vector<Polygon>, vector<Polygon>,
  --  %                                 vector<double>, double)
  --  % with the assumption that the resultingPolygons list passed in (second
  --  % argument above) is always empty.
  --  % NB: offset_polygons DOES NOT EXIST in the newer versions of visilibity online!
  --  % NB: this routine is assumed to only be called from
  --  %   CVisibilityGraph::enError CVisibilityGraph::errExpandAndMergePolygons()
  --  % where the delta values are all exactly 0.0! This also means that the return
  --  % value would always be "true", so we are ignoring that return value
  --  offset_polygons(polygonList: fs_zone_polygons, epsilon: eps_type):
  --      {f: finseq[simple_polygon_2d] | f`length <= polygonList`length} = % AKA resultingPolygons
  --    IF polygonList`length = 0 THEN
  --      empty_seq
  --    ELSE
  --      % Loop verifying delta values is omitted on assumption that all deltas are zero
  --      %  (See comment in header of this function)
  --      % NB: the line calling Polygon::boost_union exits prematurely with the boolean false
  --      %   if boost_union returns false
  --      LET simpleMerge: finseq[simple_polygon_2d] = boost_union(polygonList, epsilon) IN
  --      remove_very_small_polys(simpleMerge, epsilon)
  --    ENDIF;
  function offset_polygons(polygonList: FSFZ.FS.Finseq; epsilon: eps_type) return FSFZ.FS.Finseq is
    (if polygonList.length = 0 then
        FSFZ.FS.Empty_Seq
     else
        remove_very_small_polys(boost_union(polygonList), epsilon));

end expandMergePolygons;
