with vectors_2d;
with polygons_2d;

package Polygon_Lemmas with SPARK_Mode,
    Ghost is -- all content is ghost
  use vectors_2d;
  use polygons_2d;

  procedure Lemma_Pts_Can_Subtract_Implies_Segment_Endpts_Can_Subtract(G: polygon_2d; p: point_2d)
    with
      Global => null,
      Pre => (for all i in 0 .. G.num_vertices-1 =>
                can_subtract(G.vertices(i), p) and can_subtract(p, G.vertices(i))),
      Post => (for all i in 0 .. G.num_vertices-1 =>
                 can_subtract(edges_of_polygon(G, i).p1, p) and
                   can_subtract(edges_of_polygon(G, i).p2, p) and
                 can_subtract(p, edges_of_polygon(G, i).p1) and
                   can_subtract(p, edges_of_polygon(G, i).p2) and
                 (for all j in 0 .. G.num_vertices-1 =>
                        can_subtract(edges_of_polygon(G, i).p1, edges_of_polygon(G, j).p1) and
                    can_subtract(edges_of_polygon(G, i).p1, edges_of_polygon(G, j).p2) and
                      can_subtract(edges_of_polygon(G, i).p2, edges_of_polygon(G, j).p1) and
                    can_subtract(edges_of_polygon(G, i).p2, edges_of_polygon(G, j).p2)));

  --  procedure Lemma_Transitive_Vertex_Inclusion(p1 : polygon_2d; p2 : polygon_2d;
  --                                              n_p1 : polygon_2d;
  --                                              n_p2 : polygon_2d;
  --                                              vertices : uniq_vertex_list;
  --                                              merged : polygon_2d)
  --    with
  --      Global => null,
  --      Pre =>
  --        ((for all v of n_p1.vertices =>
  --                      in_array(v, p1.vertices) or in_array(v, vertices))
  --          and
  --            (for all v of n_p2.vertices =>
  --                          in_array(v,p2.vertices) or in_array(v,vertices))
  --          and
  --            (for all v of merged.vertices =>
  --                          in_array(v,n_p1.vertices) or in_array(v,n_p2.vertices))),
  --      Post =>
  --        (for all v of merged.vertices =>
  --          in_array(v,p1.vertices) or
  --            in_array(v,p2.vertices) or
  --            in_array(v,vertices));
end Polygon_Lemmas;
