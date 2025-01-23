-- -----------------------------------------------------------------------------
-- operating_region.ads              Dependable Computing
-- Corresponds to logic from operating_region.pvs
-- -----------------------------------------------------------------------------
with zone_polygons;
with polygons_2d;
with eps_polygons;
with findAllVisibleEdges;
with Finseq_Fns;
with Finite_Sequences;
with buildVisibilityGraph;
with expandMergePolygons;

package operating_region is
   use zone_polygons;
   use polygons_2d;
   use eps_polygons;
   use findAllVisibleEdges;
   use buildVisibilityGraph;
   use expandMergePolygons;

   -- Represents a unique identifier for polygons
   subtype Unique_ID_Type is Integer;

   type Zone_Pair is record
      ID   : Unique_ID_Type;
      Zone : zone_polygon;
   end record;

   Default_Zone_Pair: constant Zone_Pair :=
     (ID => 0, Zone => Default_Zone_Polygon);

   package FSFS renames findAllVisibleEdges.FSFS;
   package FSFZ renames findAllVisibleEdges.FSFZ;
   package FSFID is new Finseq_Fns(T => Unique_ID_Type, Default_Value => 0);
   package FSFZP is new Finseq_Fns(T => Zone_Pair, Default_Value => Default_Zone_Pair);

   type Polygon_Set(Length: Natural) is record
      Zones: FSFZP.FS.Finseq(Length => Length); -- Association list mapping IDs to polygons
   end record;

   -- PVS:
   --  polygon_set_to_fs_zone_polygons(ps: polygon_set):
   --      RECURSIVE {fs: fs_zone_polygons |
   --                   (card(ps`unique_ids) = length(fs)) AND
   --                   ((empty?(ps`unique_ids) AND length(fs) = 0) OR
   --                    eps(fs) = eps(ps))} =
   --    IF empty?(ps`unique_ids) THEN
   --      empty_seq
   --    ELSE
   --      LET id = choose(ps`unique_ids),
   --          rest_ids: finite_set[int] = rest(ps`unique_ids),
   --          rest_set =
   --            (# unique_ids := rest_ids,
   --               zones := LAMBDA(id: (rest_ids)):
   --                          ps`zones(id) #) IN
   --      prepend(ps`zones(id),
   --        polygon_set_to_fs_zone_polygons(rest_set))
   --    ENDIF
   --    MEASURE card(ps`unique_ids);
   function polygon_set_to_fs_zone_polygons(ps: Polygon_Set) return FSFZ.FS.Finseq;

   -- PVS:
   --   add_polygon(region_polygons: polygon_set, id: int,
   --               new_polygon: zone_polygon,
   --        polygon_expansion_dist: real):
   --       polygon_set =
   --     LET uniq_ids =  LAMBDA(i: int):
   --         (i = id OR region_polygons`unique_ids(i)) IN
   --     (#
   --       unique_ids := uniq_ids,
   --       zones := LAMBDA(i: (uniq_ids)):
   --         IF (i = id) THEN
   --    new_polygon
   --  ELSE
   --    region_polygons`zones(i)
   --  ENDIF
   --      #);
   function add_polygon(region_polygons: FSFZP.FS.Finseq; id: Unique_ID_Type;
                        new_polygon: zone_polygon) return FSFZP.FS.Finseq;

   -- PVS:
   --  % Generates a visibility graph from a set of zone polygons
   --  processOperatingRegion(region: polygon_set): finseq[segment_2d] =
   --    LET orig_list = polygon_set_to_fs_zone_polygons(region),
   --        offset_eps = 10^(-8), % This value is hardcoded in CVisibilityGraph::errExpandAndMergePolygons
   --        upd_list = offset_polygons(orig_list, offset_eps) IN
   --    buildVisibilityGraph(upd_list);
   function processOperatingRegion(region: Polygon_Set) return FSFS.FS.Finseq is
     (buildVisibilityGraph_fn(
      offset_polygons(
        polygon_set_to_fs_zone_polygons(region),
        1.0E-8)));

end operating_region;
