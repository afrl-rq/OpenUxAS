-- -----------------------------------------------------------------------------
-- findAllVisibleEdges.ads              Dependable Computing
-- Corresponds to logic from findAllVisibleEdges.pvs
-- -----------------------------------------------------------------------------

with expandMergePolygons;
with polygons_2d;
with zone_polygons;
with eps_polygons;
with eps_segments;
with segments_2d;
with Finseq_Fns;
with Finite_Sequences;
with vectors_2d;

package findAllVisibleEdges with SPARK_Mode is

   use expandMergePolygons;
   use polygons_2d;
   use zone_polygons;
   use eps_polygons;
   use eps_segments;
   use segments_2d;
   use vectors_2d;

   package FSFZ renames zone_polygons.FSFZ;
   package FSFP is new Finseq_Fns(T => polygon_2d, Default_Value => Eps_Square);
   package FSFS is new Finseq_Fns(T => segment_2d, Default_Value => Default_Segment);

   --PVS:
   --  is_segment_extending?(s1, s2: segment_2d): boolean =
   --    segment_intersect_kernel(s1, s2)`1 = Collinear_Overlapping AND
   --      (((s1`p1 = s2`p1) AND (^(s1) = -^(s2))) OR
   --       ((s1`p1 = s2`p2) AND (^(s1) = ^(s2))) OR
   --       ((s1`p2 = s2`p1) AND (^(s1) = ^(s2))) OR
   --       ((s1`p2 = s2`p2) AND (^(s1) = -^(s2))));
   function is_segment_extending(s1, s2: segment_2d) return Boolean is
     (segment_intersect_kernel(s1, s2).intersect_result = Collinear_Overlapping and then
          (((s1.p1 = s2.p1) and (normalize(s1) = -normalize(s2))) or
               ((s1.p1 = s2.p2) and (normalize(s1) = normalize(s2))) or
             ((s1.p2 = s2.p1) and (normalize(s1) = normalize(s2))) or
               ((s1.p2 = s2.p2) and (normalize(s1) = -normalize(s2)))));

   -- PVS:
   --  % Whether edge1 comes to a T at edge2 or vice-versa
   --  is_t_type_intersection?(edge1, edge2: segment_2d): boolean =
   --    (distance(edge1`p1, edge2) = 0 OR distance(edge1`p2, edge2) = 0 OR
   --     distance(edge2`p1, edge1) = 0 OR distance(edge2`p2, edge1) = 0)
   --    AND NOT collinear?(edge1, edge2);
   function is_t_type_intersection(edge1, edge2: segment_2d) return Boolean is
     ((distance(edge1.p1, edge2) = 0.0 or
        distance(edge1.p2, edge2) = 0.0 or
        distance(edge2.p1, edge1) = 0.0 or
        distance(edge2.p2, edge1) = 0.0) and then
        not is_collinear(edge1, edge2));

   -- PVS:
   --    segmentIntersectsPolygon_helper(edge: segment_2d, polygon: polygon_2d,
   --        idx: upto(polygon`num_vertices)): RECURSIVE boolean =
   --      IF idx = polygon`num_vertices THEN
   --        FALSE
   --      ELSE
   --        LET polygonEdge = edges_of_polygon(polygon)(idx) IN
   --  %      % This semantically matches the code in CEdge::bIntersection
   --  %      % (in file Edge.cpp), accounting for the macros
   --  %      IF vertices_touch(edge, polygonEdge) OR
   --  %          NOT are_segments_intersecting?(edge)(polygonEdge) THEN
   --  %        segmentIntersectsPolygon_helper(edge, polygon, idx+1)
   --  %      ELSE % if are_segments_intersecting? and not vertices_touch
   --  %        TRUE
   --  %      ENDIF
   --  % Fixed to be:
   --        IF are_segments_intersecting?(edge)(polygonEdge) AND NOT
   --            % Exception is single line point of either edge touching
   --  	  % but not crossing (or overlapping) the other
   --            (is_t_type_intersection?(edge, polygonEdge) OR
   --  	   is_segment_extending?(edge, polygonEdge)) THEN
   --          TRUE
   --        ELSE
   --          % Check remaining cases for intersections
   --          segmentIntersectsPolygon_helper(edge, polygon, idx+1)
   --        ENDIF
   --      ENDIF
   --      MEASURE polygon`num_vertices - idx;
   --  
   --   segmentIntersectsPolygon(edge: segment_2d, polygon: polygon_2d): boolean =
   --     segmentIntersectsPolygon_helper(edge, polygon, 0);
   function segmentIntersectsPolygon(edge: segment_2d; polygon: polygon_2d) return Boolean;

   -- PVS:
   --  intersectionFound_helper(allPolygons: fs_polygons, edge: segment_2d,
   --      idx: upto(allPolygons`length)): RECURSIVE boolean =
   --    IF idx = allPolygons`length THEN
   --      FALSE
   --    ELSIF segmentIntersectsPolygon(edge, allPolygons`seq(idx)) THEN
   --      TRUE
   --    ELSE
   --      intersectionFound_helper(allPolygons, edge, idx+1)
   --    ENDIF
   --    MEASURE allPolygons`length - idx;
   --  
   --  intersectionFound(allPolygons: fs_polygons, edge: segment_2d): boolean =
   --    intersectionFound_helper(allPolygons, edge, 0);
   function intersectionFound(allPolygons: FSFZ.FS.Finseq; edge: segment_2d) return Boolean;

   -- PVS:
   --   findVisibleEdges_inner(polygonThis, polygonThat: zone_polygon,
   --       allPolygons: (p_in_fs?(polygonThis, polygonThat)),
   --       idx1: below(num_vertices(polygonThis)),
   --       idx2: upto(num_vertices(polygonThat))):
   --     RECURSIVE finseq[segment_2d] =
   --     IF idx2 = num_vertices(polygonThat) THEN
   --       empty_seq
   --     ELSE
   --       LET v1 = v(polygonThis)(idx1),
   --           v2 = v(polygonThat)(idx2),
   --    centerPt = 0.5 * (v1 + v2),
   --    % TODO: Verify hypothesis that goodEdge is doing nothing for us, given
   --    %  the later checking for intersectionFound
   --    % NB: If it is doing something meaningful, then the order of polygons
   --    %  in the polygon list will end up affecting the result of
   --    %  buildVisibilityGraph
   --           goodEdge = (NOT polygonThis`keep_in?) OR
   --      is_point_in_polygon_inclusive?(polygonThis)(centerPt),
   --           restOfList =
   --      findVisibleEdges_inner(polygonThis, polygonThat, allPolygons, idx1, idx2+1) IN
   --       % Original code doesn't have v1 = v2 check
   --       IF v1 = v2 OR NOT goodEdge THEN
   --         restOfList
   --       ELSE
   --         LET edgeNew = (# p1 := v1, p2 := v2 #) IN
   --  IF intersectionFound(allPolygons, edgeNew) THEN
   --           restOfList
   --  ELSE
   --    prepend_segment_list(edgeNew, restOfList)
   --  ENDIF
   --       ENDIF
   --     ENDIF
   --     MEASURE num_vertices(polygonThat) - idx2;
   --  
   --   findVisibleEdges_outer(polygonThis, polygonThat: zone_polygon,
   --       allPolygons: (p_in_fs?(polygonThis, polygonThat)),
   --       idx1: upto(num_vertices(polygonThis))):
   --       RECURSIVE finseq[segment_2d] =
   --     IF idx1 = num_vertices(polygonThis) THEN
   --       empty_seq
   --     ELSE
   --       findVisibleEdges_inner(polygonThis, polygonThat, allPolygons, idx1, 0) o
   --         findVisibleEdges_outer(polygonThis, polygonThat, allPolygons, idx1+1)
   --     ENDIF
   --     MEASURE num_vertices(polygonThis) - idx1;
   --  
   --   %Pseudocode from original C++:
   --   % visibleEdges = emptyList
   --   % for v1 in polygonThis:
   --   %   for v2 in polygonThat:
   --   %     goodEdge = true # default to edge being good for keep out zones
   --   % 	if polygonThis.keepIn
   --   % 	  centerPt = (v1 + v2) / 2
   --   % 	  goodEdge = polygonThis.isInside(centerPt)
   --   % 	if goodEdge then
   --   % 	  edgeNew = segment(v1, v2)
   --   % 	  intersectionFound = false
   --   % 	  for poly in allPolygons
   --   % 	    if poly.intersect(edgeNew)
   --   %           intersectionFound = true
   --   % 	      break
   --   % 	  if not intersectionFound
   --   % 	    visibleEdges.add(edgeNew)
   --   findVisibleEdges(polygonThis, polygonThat: zone_polygon,
   --       allPolygons: (p_in_fs?(polygonThis, polygonThat))):
   --       finseq[segment_2d] =
   --     findVisibleEdges_outer(polygonThis, polygonThat, allPolygons, 0);
   function findVisibleEdges(polygonThis, polygonThat: zone_polygon;
                             allPolygons: FSFZ.FS.Finseq) return FSFS.FS.Finseq
     with
       Pre => FSFZ.Is_In(allPolygons, polygonThis) and
       FSFZ.Is_In(allPolygons, polygonThat);

   -- PVS:
   --  findAllVisibleEdges(p: zone_polygon,
   --        mergedPolygons: (zp_in_fs?(p)),
   --        idx: upto(mergedPolygonslength)):
   --      RECURSIVE finseq[segment_2d] =
   --    IF idx = mergedPolygonslength THEN
   --      empty_seq
   --    ELSE
   --      LET that: zone_polygon = mergedPolygons`seq(idx) IN
   --      findVisibleEdges(p, that, mergedPolygons) o
   --        findAllVisibleEdges(p, mergedPolygons, idx+1)
   --    ENDIF
   --    MEASURE mergedPolygonslength-idx;
   function findAllVisibleEdges(p: zone_polygon; mergedPolygons: FSFZ.FS.Finseq) return FSFS.FS.Finseq
     with
       Pre => FSFZ.Is_In(mergedPolygons, p);

end findAllVisibleEdges;
