-- -----------------------------------------------------------------------------
-- polygon_snapping.ads              Dependable Computing
-- Corresponds to logic from polygon_snapping.pvs
-- -----------------------------------------------------------------------------
with polygons_2d;
with vectors_2d;
with eps_segments;
with eps_polygons;
with perimeter_props;
with vertex_list;
with Finite_Sequences;
with Finseq_Fns;
with prelude;
with zone_polygons;

package polygon_snapping with SPARK_Mode is

   use polygons_2d;
   use vectors_2d;
   use eps_segments;
   use eps_polygons;
   use perimeter_props;
   use vertex_list;
   use prelude;

   package FSFF is new Finseq_Fns(T => Float, Default_Value => 0.0);
   package FSFP renames zone_polygons.FSFP;

   -- PVS:
   --  snap_to_boundary_of(p: point_2d, environment_temp: polygon_2d, eps: eps_type): point_2d =
   --    LET pointemp = projection_onto_boundary_of(p, environment_temp) IN
   --    IF distance(p, pointemp) <= eps THEN
   --      pointemp
   --    ELSE
   --      p
   --    ENDIF;
   function snap_to_boundary_of(p: point_2d; environment_temp : polygon_2d; eps : eps_type) return point_2d
     with
       Post =>
         (if distance(p, projection_onto_boundary_of(p, environment_temp)) <= eps then
            snap_to_boundary_of'Result =
              projection_onto_boundary_of(p, environment_temp)
            else
              snap_to_boundary_of'Result = p);

   -- PVS:
   --  % This predicate is a sufficient condition for the polygon snapping
   --  % operation to produce a valid simple polygon.
   --  snapping_eligible?(epsilon: eps_type)(A: simple_polygon_2d): boolean =
   --    FORALL (i, j: below(A`num_vertices)):
   --      i = j OR i = next_index(A, j) OR
   --      distance(A`vertices(i), edges_of_polygon(A)(j)) > 2 * epsilon
   function is_snapping_eligible(epsilon : eps_type; A : simple_polygon_2d) return Boolean is
     (for all i in 0 .. A.num_vertices - 1 =>
        (for all j in 0 .. A.num_vertices - 1 =>
             (i = j or
                  i = next_index(A, j) or
                    distance(A.vertices(i), edges_of_polygon(A, j)) > 2.0 * epsilon)));

   -- PVS:
   --  snapped_vertex_list(epsilon: eps_type, A: snappable_polygon(epsilon),
   --                      B: simple_polygon_2d):
   --       uniq_vertex_list(Anum_vertices) =
   --    LAMBDA (idx: below(Anum_vertices)):
   --      IF boundary_distance(Avertices(idx), B) < epsilon
   --        THEN snap_to_boundary_of(Avertices(idx), B, epsilon)
   --        ELSE Avertices(idx)
   --      ENDIF;
   function snapped_vertex_list(epsilon : eps_type; A, B : simple_polygon_2d)
                                return uniq_vertex_list
     with
       Pre => is_snapping_eligible(epsilon, A),
       Post =>
         snapped_vertex_list'Result.num_vertices = A.num_vertices and then
         uniq_vertex_list_pred(snapped_vertex_list'Result.num_vertices,
                               snapped_vertex_list'Result.vertices);

   -- PVS:
   --   % This is a compact expression of the snapping operation.
   --   snap_vertices_to_polygon(epsilon: eps_type,
   --                            A: snappable_polygon(epsilon),
   -- 			   B: simple_polygon_2d): polygon_2d =
   --        (#
   --          num_vertices := A`num_vertices,
   --          vertices     := snapped_vertex_list(epsilon, A, B)
   --         #)
   function snap_vertices_to_polygon(epsilon: eps_type; A, B: simple_polygon_2d)
                                     return polygon_2d is
     ((num_vertices => A.num_vertices,
       vertices => snapped_vertex_list(epsilon, A, B).vertices))
     with
       Pre => is_snapping_eligible(epsilon, A);

   -- PVS:
   --  % Following definitions support the concept of a snappable polygon
   --  % sequence based on the notion of a "snap margin".
   --  vtx_to_edge_distances(A: simple_polygon_2d): finite_set[nnreal] =
   --    {d: nnreal | EXISTS (i, j: below(A`num_vertices)):
   --                   i /= j AND i /= next_index(A, j) AND
   --                   d = distance(A`vertices(i), edges_of_polygon(A)(j))}
   function vtx_to_edge_distances(A: Simple_Polygon_2D) return FSFF.FS.Finseq;

   -- PVS:
   --  % The margin is the minimum distance of a vertex to all the other
   --  % (nonadjacent) edges of the polygon.
   --  snap_margin(A: simple_polygon_2d): nnreal =
   --    min[nnreal, <=](vtx_to_edge_distances(A))
   function snap_margin(A: simple_polygon_2d) return nn_float;

   -- PVS:
   --  % This type allows treatment of the margins as ghost variables.
   --  % The field "marg" gives the number of multiples of 2 epsilon
   --  % that the margin is expected to exceed.
   --  polygon_with_margin: TYPE = [# poly: simple_polygon_2d, marg: nat #]
   type polygon_with_margin is record
      poly: simple_polygon_2d;
      marg: Natural;
   end record;

   Default_Poly_With_Margin: constant polygon_with_margin :=
     (poly => Eps_Square, marg => 0);

   -- PVS:
   --  polygon_seq_with_snap_margins: TYPE = finseq[polygon_with_margin]
   package FSFPSM is new Finseq_Fns(T => polygon_with_margin,
                                  Default_Value => Default_Poly_With_Margin);

   -- PVS:
   --  % A snappable polygon sequence has margins that exceed the values
   --  % given by the "marg" fields in the polygon_with_margin records.
   --  snappable_polygon_seq(epsilon: eps_type): TYPE =
   --    {S: polygon_seq_with_snap_margins |
   --        FORALL (i: below(S`length)):
   --   snap_margin(S`seq(i)`poly) > 2 * epsilon * S`seq(i)`marg}
   function is_snappable_polygon_seq(S: FSFPSM.FS.Finseq; epsilon: eps_type) return Boolean is
     (for all I in 0 .. S.Length - 1 =>
         snap_margin(S.Seq(I).poly) > 2.0 * epsilon * Float(S.Seq(I).marg));

   -- PVS:
   --   % Currently, this function tests each polygon before snapping to ensure
   --   % that it meets the sufficient condition expressed by "snapping_eligible?".
   --   % If not met, the original, unmodified polygon is used instead.
   --   snap_near_inner_fn(epsilon: eps_type,
   --                      polygonList: finseq[simple_polygon_2d],
   --                      idx1: below(polygonList`length),
   --                      idx2: subrange(idx1 + 1, polygonList`length - 1)):
   --       {fs: finseq[simple_polygon_2d] | fs`length = polygonList`length} =
   --     LET len = polygonList`length,
   --         modifyingList = polygonList IN
   --       IF (distance(polygonList(idx1), polygonList(idx2)) > 0 AND
   --           distance(polygonList(idx1), polygonList(idx2)) < epsilon) THEN
   --  (# length := len,
   --     seq :=
   --              LET poly1 = modifyingList(idx1),
   --           poly2 = modifyingList(idx2) IN
   --         LAMBDA(i: below(len)):
   --  	  IF (i = idx1) THEN
   --  	    IF snapping_eligible?(epsilon)(poly1)
   --  	      THEN snap_vertices_to_polygon(epsilon, poly1, poly2)
   --                       ELSE poly1
   --  	    ENDIF
   --  	  ELSIF (i = idx2) THEN
   --  	    IF snapping_eligible?(epsilon)(poly2)
   --  	      THEN snap_vertices_to_polygon(epsilon, poly2, poly1)
   --                       ELSE poly2
   --  	    ENDIF
   --  	  ELSE
   --  	    modifyingList`seq(i)
   --  	  ENDIF
   --          #)
   --       ELSE
   --         modifyingList
   --       ENDIF;
   function snap_near_inner_fn(epsilon: eps_type; polygonList: FSFP.FS.Finseq;
                               idx1: Natural; idx2: Natural) return FSFP.FS.Finseq
     with
       Pre => (idx1 < polygonList.Length and then
                 idx2 > idx1 and then
                   idx2 < polygonList.Length);

   -- PVS:
   --  snap_near_inner_loop(epsilon: eps_type,
   --                       polygonList: finseq[simple_polygon_2d],
   --                       idx1: below(polygonList`length),
   --         n: below(polygonList`length - idx1)):
   --      RECURSIVE {fs: finseq[simple_polygon_2d] |
   --                 fs`length = polygonList`length} =
   --    LET len = polygonList`length, idx2 = len - n IN
   --      IF n = 0 THEN polygonList
   --      ELSE LET snap2 = snap_near_inner_fn(epsilon, polygonList, idx1, idx2)
   --           IN  snap_near_inner_loop(epsilon, snap2, idx1, n - 1)
   --      ENDIF
   --    MEASURE n;
   function snap_near_inner_loop(epsilon: eps_type; polygonList: FSFP.FS.Finseq;
                                 idx1, n: Natural) return FSFP.FS.Finseq
     with
       Pre => (idx1 < polygonList.Length and then
                 n < polygonList.Length - idx1);

   -- PVS:
   --  snap_near_outer_loop(epsilon: eps_type,
   --                       polygonList: finseq[simple_polygon_2d],
   --                       idx1: below(polygonList`length)):
   --      RECURSIVE {fs: finseq[simple_polygon_2d] |
   --                 fs`length = polygonList`length} =
   --    LET len = polygonList`length IN
   --      IF idx1 = len - 1 THEN polygonList
   --      ELSE LET snap_inner = snap_near_inner_loop(epsilon, polygonList,
   --  		                 idx1, len - idx1 - 1)
   --           IN  snap_near_outer_loop(epsilon, snap_inner, idx1 + 1)
   --      ENDIF
   --    MEASURE polygonList`length - idx1;
   function snap_near_outer_loop(epsilon: eps_type; polygonList: FSFP.FS.Finseq;
                                 idx1: Natural) return FSFP.FS.Finseq
     with Pre => (idx1 < polygonList.Length);

   -- PVS:
   --  % This is the top-level function that snaps each polygon on a list
   --  % against all the others on the list.
   --  snap_near(epsilon: eps_type,
   --            polygonList: finseq[simple_polygon_2d]):
   --      {f: finseq[simple_polygon_2d] | f`length = polygonList`length} =
   --    LET len = polygonList`length IN
   --    % NB: PVS does not have a concept of "pass-by-reference", so think of
   --    %     each polygonList as being a *copy* of the original polygonList
   --    IF len = 0
   --      THEN polygonList
   --      ELSE snap_near_outer_loop(epsilon, polygonList, 0)
   --    ENDIF;
   function snap_near(epsilon: eps_type; polygonList: FSFP.FS.Finseq) return FSFP.FS.Finseq is
     (if polygonList.Length = 0
      then polygonList
      else snap_near_outer_loop(epsilon, polygonList, 0)
     );

end polygon_snapping;
