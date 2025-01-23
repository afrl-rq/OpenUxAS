-- -----------------------------------------------------------------------------
-- buildVisibilityGraph.ads              Dependable Computing
-- Corresponds to logic from buildVisibilityGraph.pvs
-- -----------------------------------------------------------------------------

with polygons_2d;
with segments_2d;
with Finite_Sequences;
with Finseq_Fns;
with eps_segments;
with vectors_2d;
with zone_polygons;
with findAllVisibleEdges;
with vertex_list;

package buildVisibilityGraph with SPARK_Mode is

   use segments_2d;
   use eps_segments;
   use vectors_2d;
   use zone_polygons;
   use findAllVisibleEdges;
   use vertex_list;
   use polygons_2d;

   package FSFP is new Finseq_Fns(T => point_2d, Default_Value => zero_point);
   package FSFS renames findAllVisibleEdges.FSFS;
   package FSFW is new Finseq_Fns(T => weak_segment_2d, Default_Value => Default_Segment);
   package FSFZ renames findAllVisibleEdges.FSFZ;

   -- PVS:
   --  valid_segment?(w: weak_segment_2d): boolean =
   --    w`p1 /= w`p2;
   function is_valid_segment(w: weak_segment_2d) return Boolean is
      (w.p1 /= w.p2);

   -- PVS:
   --  to_segment_2d(s: (valid_segment?)): segment_2d =
   --    (# p1 := s`p1, p2 := s`p2 #);
   function to_segment_2d(w: weak_segment_2d) return segment_2d is
     ((p1 => w.p1, p2 => w.p2))
       with
         Pre => is_valid_segment(w);

   -- PVS:
   --  strip_weak(fsw: finseq[weak_segment_2d]):
   --      RECURSIVE finseq[segment_2d] =
   --    IF length(fsw) = 0 THEN
   --      empty_seq
   --    ELSE
   --      LET first = car(fsw),
   --          rest = cdr(fsw) IN
   --      IF valid_segment?(first) THEN
   --        prepend(to_segment_2d(first), strip_weak(rest))
   --      ELSE
   --        strip_weak(rest)
   --      ENDIF
   --    ENDIF
   --    MEASURE length(fsw);
   function strip_weak(fsw: FSFW.FS.Finseq) return FSFS.FS.Finseq;

   type Merge_Result(Length: Natural) is record
      Polygon : zone_polygon;
      Other_Polygons : FSFZ.FS.Finseq(Length => Length);
   end record;

   function mergePolygonWithAllMergeablePolygons(p : zone_polygon; other_polys : FSFZ.FS.Finseq) 
                                                 return Merge_Result
     with
       Pre => is_fs_zone_polygons(other_polys) and then other_polys.Length > 0 and then p.eps = eps(other_polys),
     Post => mergePolygonWithAllMergeablePolygons'Result.Polygon.eps = p.eps and
     mergePolygonWithAllMergeablePolygons'Result.Other_Polygons.Length <= other_polys.Length and
     (mergePolygonWithAllMergeablePolygons'Result.Other_Polygons.Length = 0 or
        (is_fs_zone_polygons(mergePolygonWithAllMergeablePolygons'Result.Other_Polygons) and then
             eps(mergePolygonWithAllMergeablePolygons'Result.Other_Polygons) = eps(other_polys)));

   function mergeMergeablePolygons(allPolygons: FSFZ.FS.Finseq) return FSFZ.FS.Finseq;

   -- PVS:
   -- % Whether an edge (or edge portion) is inside at least one keep-in zone
   -- % and outside all keep-out zones. Assumes that all mergeable polygons
   -- % have been merged already, so if there is more than one keep-in zone,
   -- % they must be disjoint.
   -- isEdgeGood?(polygons: fs_zone_polygons)(edge: segment_2d): boolean =
   --   % The edge does not intersect or overlap any edge from any polygon
   --   % unless a vertex from the edge is collocated with a vertex from
   --   % that same edge in question
   --   NOT intersectionFound(polygons, edge) AND
   --   (EXISTS(i: below(polygons`length)):
   --      LET zp: zone_polygon = polygons`seq(i) IN
   --      zp`keep_in? AND
   --      is_point_in_polygon_exclusive?(zp)(segment_midpoint(edge))) AND
   --   (NOT EXISTS(i: below(polygons`length)):
   --      LET zp: zone_polygon = polygons`seq(i) IN
   --      (NOT zp`keep_in?) AND
   --      is_point_in_polygon_exclusive?(zp)(segment_midpoint(edge)));
   function isEdgeGood(polygons: FSFZ.FS.Finseq; edge: segment_2d) return Boolean;

   -- PVS:
   -- goodEdgePortions(edgePortions: finseq[segment_2d],
   --                  others: fs_zone_polygons): RECURSIVE finseq[segment_2d] =
   --   IF edgePortions = empty_seq THEN
   --     empty_seq
   --   ELSE
   --     LET edgePortion: segment_2d = car(edgePortions),
   --         rest: finseq[segment_2d] = cdr(edgePortions),
   --         % By construction, portions cannot intersect other polygons
   --         % other than to touch them
   --         % isEdgeGood? is only valid here because others excludes the
   --         % polygon from which the edgePortion is constructed
   --         % Also, this logic relies on all mergeable polygons having
   --         % already been merged
   --         goodPortion = isEdgeGood?(others)(edgePortion) IN
   --     IF goodPortion THEN
   --       prepend(edgePortion, goodEdgePortions(rest, others))
   --     ELSE
   --       goodEdgePortions(rest, others)
   --     ENDIF
   --   ENDIF
   --   MEASURE edgePortions`length;
   function goodEdgePortions(edgePortions: FSFS.FS.Finseq; Other_Polygons: FSFZ.FS.Finseq) return FSFS.FS.Finseq;

   -- PVS:
   -- portionEdge(edge: segment_2d, others: fs_zone_polygons):
   --   RECURSIVE {Q: finite_set[point_2d] |
   --                FORALL (p: (Q)): is_point_on_segment?(edge)(p)} =
   --   IF others = empty_seq THEN
   --     emptyset
   --   ELSE
   --     union(injected_edge(edge, car(others)),
   --           portionEdge(edge, cdr(others)))
   --   ENDIF
   --   MEASURE others`length;

   -- Finds all intersections of edge with other polygons
   function portionEdge(edge: segment_2d; Other_Polygons: FSFZ.FS.Finseq) return uniq_vertex_list;

   -- PVS:
   -- buildEdgePortions(edge: segment_2d,
   --                   edgePts: {Q: finite_set[point_2d] |
   --                    FORALL (p: (Q)): is_point_on_segment?(edge)(p)}):
   --   {Q: finite_set[segment_2d] | FORALL(s: (Q)):
   --     is_point_on_segment?(edge)(s`p1) AND
   --     is_point_on_segment?(edge)(s`p2) AND
   --     (FORALL(p: (edgePts)):
   --        p = s`p1 OR p = s`p2 OR NOT is_point_on_segment?(s)(p))} =
   -- {s: segment_2d | member(s`p1, edgePts) AND member(s`p2, edgePts) AND
   --                   (FORALL(p: (edgePts)): p = s`p1 OR p = s`p2 OR
   --                      NOT is_point_on_segment?(s)(p))};
   -- Constructs edge portions from edge points, ensuring no duplicates
   function buildEdgePortions(edge: segment_2d; edgePts: uniq_vertex_list) return FSFS.FS.Finseq;

   -- PVS:
   -- addGoodEdgesFromEdges(edges: finseq[segment_2d],
   --                       others: fs_zone_polygons): RECURSIVE finseq[segment_2d] =
   --   IF edges = empty_seq THEN
   --     empty_seq
   --   ELSE
   --     LET edge: segment_2d = car(edges),
   --         rest: finseq[segment_2d] = cdr(edges),
   --         goodEdge = isEdgeGood?(others)(edge) IN
   --     IF goodEdge THEN
   --       prepend(edge, addGoodEdgesFromEdges(rest, others))
   --     ELSE
   --       % At this point, we know the whole edge isn't good, but portions of the edge
   --       % might be. If the edge is coming from a keep-in zone, then portions of the
   --       % edge are good if and only if they intersect any keep-out zone. If the edge
   --       % is coming from a keep-out zone, then portions of the edge are good, if and
   --       % only if they intersect with a keep-in zone. Note that it's possible for an
   --       % edge to start inside a zone, intersect with more than one edge and end
   --       % inside the same zone or a different zone. Presumably, after merging there
   --       % will only be one keep-in zone, but we're not enforcing that here. It's also
   --       % possible that there will be more than one portion of the edge that is good.
   --       LET edgePts: {Q: finite_set[point_2d] |
   --                      FORALL (p: (Q)): is_point_on_segment?(edge)(p)} =
   --             % edge`p1 is included in portionEdge, but not edge`p2
   --             add(edge`p2, portionEdge(edge, others)),
   --         edgePortions = set2seq(buildEdgePortions(edge, edgePts)) IN
   --       goodEdgePortions(edgePortions, others) o
   --         addGoodEdgesFromEdges(rest, others)
   --     ENDIF
   --   ENDIF
   --   MEASURE edges`length;
   function addGoodEdgesFromEdges(edges: FSFS.FS.Finseq; Other_Polygons: FSFZ.FS.Finseq) return FSFS.FS.Finseq;

   -- Converts polygons_2d's edges_of_polygon from having an index argument to being a finite sequence
   -- Also uses zone_polygon instead of polygon_2d
   function edges_of_polygon(p: zone_polygon) return FSFS.FS.Finseq;

   -- PVS:
   --  addGoodEdgesFromPolygon(p: zone_polygon, others: fs_zone_polygons): finseq[segment_2d] =
   --    LET edges: finseq[segment_2d] =
   --      (# length := num_vertices(p),
   --         seq := edges_of_polygon(p) #) IN
   --    addGoodEdgesFromEdges(edges, others);
   function addGoodEdgesFromPolygon(p: zone_polygon; Other_Polygons: FSFZ.FS.Finseq) return FSFS.FS.Finseq is
      (addGoodEdgesFromEdges(edges_of_polygon(p), Other_Polygons));

   -- PVS:
   -- addGoodEdges_helper(mergedPolygons: fs_zone_polygons,
   --                     idx: upto(mergedPolygons`length)):
   --   RECURSIVE finseq[segment_2d] =
   --   IF idx = mergedPolygons`length THEN
   --     empty_seq
   --   ELSE
   --     LET p = mergedPolygons`seq(idx),
   --         others = remove(mergedPolygons, idx) IN
   --     addGoodEdgesFromPolygon(p, others) o
   --       addGoodEdges_helper(mergedPolygons, idx+1)
   --   ENDIF
   --   MEASURE mergedPolygons`length - idx;
   --
   -- addGoodEdges(mergedPolygons: fs_zone_polygons): finseq[segment_2d] =
   --   addGoodEdges_helper(mergedPolygons, 0);
   function addGoodEdges(mergedPolygons: FSFZ.FS.Finseq) return FSFS.FS.Finseq;

   -- PVS:
   -- findGoodConnections(edge: segment_2d, allPolygons: fs_zone_polygons,
   --                     remainingEdges: finseq[segment_2d]):
   --   RECURSIVE finseq[segment_2d] =
   --   IF length(remainingEdges) = 0 THEN
   --     empty_seq
   --   ELSE
   --     LET
   --       other = car(remainingEdges),
   --       rest = cdr(remainingEdges),
   --       potentials: finseq[weak_segment_2d] =
   --       (# length := 4,
   --          seq := (LAMBDA(i: below(4)):
   --            COND
   --              i = 0 ->
   --                (# p1 := edge`p1, p2 := other`p1 #),
   --              i = 1 ->
   --                (# p1 := edge`p1, p2 := other`p2 #),
   --              i = 2 ->
   --                (# p1 := edge`p2, p2 := other`p1 #),
   --              i = 3 ->
   --                (# p1 := edge`p2, p2 := other`p2 #)
   --            ENDCOND) #) IN
   --       goodEdgePortions(strip_weak(potentials), allPolygons)
   --   ENDIF
   --   MEASURE length(remainingEdges);
   function findGoodConnections(edge: segment_2d; allPolygons: FSFZ.FS.Finseq; remainingEdges: FSFS.FS.Finseq) return FSFS.FS.Finseq;

   -- PVS:
   -- includeGoodConnections(allPolygons: fs_zone_polygons,
   --                        perimeterEdges: finseq[segment_2d]):
   --   RECURSIVE finseq[segment_2d] =
   --   IF length(perimeterEdges) = 0 THEN
   --     empty_seq
   --   ELSE
   --     LET edge = car(perimeterEdges),
   --         rest = cdr(perimeterEdges) IN
   --     % We don't need to look at all perimeter edges due to edges that
   --     % came before this one already being checked against this one
   --     findGoodConnections(edge, allPolygons, rest) o
   --       includeGoodConnections(allPolygons, rest)
   --   ENDIF
   --   MEASURE length(perimeterEdges);
   function includeGoodConnections(allPolygons: FSFZ.FS.Finseq; perimeterEdges: FSFS.FS.Finseq) return FSFS.FS.Finseq;

   -- PVS:
   -- buildVisibilityGraph(allPolygons: fs_zone_polygons): finseq[segment_2d] =
   --   LET mergedPolygons = mergeMergeablePolygons(allPolygons),
   --       perimeterEdges = addGoodEdges(mergedPolygons) IN
   --   perimeterEdges o includeGoodConnections(mergedPolygons, perimeterEdges);
   function buildVisibilityGraph_fn(allPolygons: FSFZ.FS.Finseq) return FSFS.FS.Finseq is
      (FSFS.FS.Concat(
         addGoodEdges(mergeMergeablePolygons(allPolygons)),
         includeGoodConnections(
            mergeMergeablePolygons(allPolygons),
         addGoodEdges(mergeMergeablePolygons(allPolygons)))));

end buildVisibilityGraph;
