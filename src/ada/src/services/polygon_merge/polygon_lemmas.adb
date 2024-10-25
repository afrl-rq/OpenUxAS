package body Polygon_Lemmas with SPARK_mode is

--  pragma Warnings
--    (Off, "postcondition does not check the outcome of calling");

  procedure Lemma_Pts_Can_Subtract_Implies_Segment_Endpts_Can_Subtract(G: polygon_2d; p: point_2d)
  is begin
    -- From the precondition:
    --   for all i in 0 .. G.num_vertices-1 =>
    --               can_subtract(G.vertices(i), p) and can_subtract(p, G.vertices(i))
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    can_subtract(G.vertices(i), p) and can_subtract(p, G.vertices(i)));
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    edges_of_polygon(G, i).p1 = G.vertices(i) and
                    edges_of_polygon(G, i).p2 = G.vertices(next_index(G, i)));
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    next_index(G, i) =
                  (if i = G.num_vertices-1 then
                       0
                     else
                       i+1));
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    edges_of_polygon(G, i).p1 = G.vertices(i) and
                    edges_of_polygon(G, i).p2 = G.vertices(
                    (if i = G.num_vertices-1 then
                      0
                    else
                      i+1)));
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    (-- from pragma Assert at line 11
                     can_subtract(G.vertices(i), p) and then can_subtract(p, G.vertices(i)) and then
                     -- from pragma Assert at line 22
                     edges_of_polygon(G, i).p1 = G.vertices(i) and then
                     -- inverting above
                     G.vertices(i) = edges_of_polygon(G, i).p1 and then
                     can_subtract(edges_of_polygon(G, i).p1, p) and then
                     can_subtract(p, edges_of_polygon(G, i).p1)
                    ));
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    (can_subtract(edges_of_polygon(G, i).p1, p) and
                       can_subtract(p, edges_of_polygon(G, i).p1)
                    ));
    -- The postcondition
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    (can_subtract(edges_of_polygon(G, i).p1, p) and
                       can_subtract(edges_of_polygon(G, i).p2, p) and
                         can_subtract(p, edges_of_polygon(G, i).p1) and
                       can_subtract(p, edges_of_polygon(G, i).p2)
                    ));
    pragma Assert(polygon_2d_constraint(G));
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    (for all j in 0 .. G.num_vertices-1 =>
                       (can_subtract(G.vertices(j), G.vertices(i)))));
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    (for all j in 0 .. G.num_vertices-1 =>
                       (can_subtract(G.vertices(i), G.vertices(j)))));
    -- FAN Assert 1
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    (for all j in 0 .. G.num_vertices-1 =>
                       (can_subtract(G.vertices(i), G.vertices(j)) and
                            can_subtract(G.vertices(i), G.vertices(next_index(G, j))) and
                          can_subtract(G.vertices(next_index(G, i)), G.vertices(j)) and
                            can_subtract(G.vertices(next_index(G, i)), G.vertices(next_index(G, j)))
                       )));
    -- FAN Assert 2
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    G.vertices(i) = edges_of_polygon(G, i).p1);
    -- FAN Assert 3
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    G.vertices(next_index(G, i)) = edges_of_polygon(G, i).p2);
    -- FAN Argument
    --  Believing
    --    The following Assume is true
    --  is justified by
    --    Using substitution
    --  with
    --    FAN Assert 2 and FAN Assert 3 instantiated for both i and j
    --  to
    --    FAN Assert 1
    --  end
    pragma Assume(for all i in 0 .. G.num_vertices-1 =>
                    (for all j in 0 .. G.num_vertices-1 =>
                       (can_subtract(edges_of_polygon(G, i).p1, edges_of_polygon(G, j).p1) and
                            can_subtract(edges_of_polygon(G, i).p1, edges_of_polygon(G, j).p2) and
                          can_subtract(edges_of_polygon(G, i).p2, edges_of_polygon(G, j).p1) and
                            can_subtract(edges_of_polygon(G, i).p2, edges_of_polygon(G, j).p2)
                       )));
  end Lemma_Pts_Can_Subtract_Implies_Segment_Endpts_Can_Subtract;

  --  procedure Lemma_Transitive_Vertex_Inclusion(p1: polygon_2d; p2: polygon_2d;
  --                                              n_p1 : polygon_2d;
  --                                              n_p2 : polygon_2d;
  --                                              vertices : uniq_vertex_list; merged : polygon_2d)
  --  is null;

end Polygon_Lemmas;
