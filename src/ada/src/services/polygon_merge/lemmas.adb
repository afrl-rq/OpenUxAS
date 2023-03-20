with prelude;

package body Lemmas with SPARK_mode is

  use prelude;

--  pragma Warnings
--    (Off, "postcondition does not check the outcome of calling");

  procedure Lemma_NonNeg_Mult_By_O_to_1_LE_Orig(x: Float; t: Float)
  is begin
    both_sides_times_pos_le1_imp(t, 1.0, x);
  end Lemma_NonNeg_Mult_By_O_to_1_LE_Orig;

  procedure Lemma_Neg_Mult_By_O_to_1_GE_Orig(x: Float; t: Float)
  is begin
    both_sides_times_neg_le1_imp(1.0, t, x);
  end Lemma_Neg_Mult_By_O_to_1_GE_Orig;

  procedure Lemma_Vector_2d_Constraint_Even(x: vector_float_type; y: vector_float_type)
  is begin
    -- ((abs(x) < 1.0 or else abs(x) <= vector_float_type'Last / abs(x)) and then
    --  (abs(y) < 1.0 or else abs(y) <= vector_float_type'Last / abs(y)) and then
    --  (x * x <= vector_float_type'Last - y * y))
    if (abs(x) < 1.0) then
      pragma Assert(abs(-x) < 1.0);
    else
      pragma Assert(abs(-x) <= vector_float_type'Last / abs(-x));
    end if;
    if (abs(y) < 1.0) then
      pragma Assert(abs(-y) < 1.0);
    else
      pragma Assert(abs(-y) <= vector_float_type'Last / abs(-y));
    end if;
    pragma Assert((-x) * (-x) <= vector_float_type'Last - (-y) * (-y));
  end Lemma_Vector_2d_Constraint_Even;

  procedure Lemma_Can_Add_Symmetric(a: vector_float_type; b: vector_float_type)
  is null;

  procedure Lemma_Can_Add_Even(a: vector_float_type; b: vector_float_type)
  is null;

  procedure Lemma_Can_Subtract_Symmetric(p1: vector_2d; p2: vector_2d)
  is begin
    -- can_add(p1.x, -p2.x)
    Lemma_Can_Add_Symmetric(p1.x, -p2.x);
    Lemma_Can_Add_Even(-p2.x, p1.x);
    pragma Assert(can_add(p2.x, -p1.x));
    -- can_add(p1.y, -p2.y)
    Lemma_Can_Add_Symmetric(p1.y, -p2.y);
    Lemma_Can_Add_Even(-p2.y, p1.y);
    pragma Assert(can_add(p2.y, -p1.y));
    -- vector_2d_constraint(p1.x - p2.x, p1.y - p2.y))
    Lemma_Vector_2d_Constraint_Even(p1.x - p2.x, p1.y - p2.y);
    pragma Assert(vector_2d_constraint(-(p1.x - p2.x), -(p1.y - p2.y)));
    pragma Assert(-(p1.x - p2.x) = -p1.x + p2.x);
    pragma Assert(-(p1.y - p2.y) = -p1.y + p2.y);
    pragma Assert(vector_2d_constraint(-p1.x + p2.x, -p1.y + p2.y));
    pragma Assert(vector_2d_constraint(p2.x - p1.x, p2.y - p1.y));
  end Lemma_Can_Subtract_Symmetric;

  procedure Lemma_Segments_Comparable_Symmetric(s1: in segment_2d; s2: in segment_2d)
  is begin
    -- (can_subtract(s1.p1, s2.p1) and can_subtract(s1.p1, s2.p2) and
    --  can_subtract(s1.p2, s2.p1) and can_subtract(s1.p2, s2.p2));
    Lemma_Can_Subtract_Symmetric(s1.p1, s2.p1);
    pragma Assert(can_subtract(s2.p1, s1.p1));
    Lemma_Can_Subtract_Symmetric(s1.p2, s2.p1);
    pragma Assert(can_subtract(s2.p1, s1.p2));
    Lemma_Can_Subtract_Symmetric(s1.p1, s2.p2);
    pragma Assert(can_subtract(s2.p2, s1.p1));
    Lemma_Can_Subtract_Symmetric(s1.p2, s2.p2);
    pragma Assert(can_subtract(s2.p2, s1.p2));
  end Lemma_Segments_Comparable_Symmetric;

  procedure Lemma_Segments_Intersect_Implies_Intersection_Can_Subtract(s1: in segment_2d; s2: in segment_2d)
  is null;

  procedure Lemma_Pts_Can_Subtract_Implies_Segment_Endpts_Can_Subtract(G: polygon_2d; p: point_2d)
  is begin
    -- From the precondition
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    can_subtract(G.vertices(i), p) and can_subtract(p, G.vertices(i)));
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    edges_of_polygon(G, i).p1 = G.vertices(i) and
                    edges_of_polygon(G, i).p2 =
                  (if i = G.num_vertices-1 then
                       G.vertices(0)
                     else
                       G.vertices(i+1)));
    -- The postcondition
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    can_subtract(edges_of_polygon(G, i).p1, p) and
                      can_subtract(edges_of_polygon(G, i).p2, p) and
                    can_subtract(p, edges_of_polygon(G, i).p1) and
                      can_subtract(p, edges_of_polygon(G, i).p2));
    pragma Assert(for all i in 0 .. G.num_vertices-1 =>
                    (for all j in 0 .. G.num_vertices-1 =>
                       can_subtract(edges_of_polygon(G, i).p1, edges_of_polygon(G, j).p1) and
                         can_subtract(edges_of_polygon(G, i).p1, edges_of_polygon(G, j).p2) and
                       can_subtract(edges_of_polygon(G, i).p2, edges_of_polygon(G, j).p1) and
                         can_subtract(edges_of_polygon(G, i).p2, edges_of_polygon(G, j).p2)));
  end Lemma_Pts_Can_Subtract_Implies_Segment_Endpts_Can_Subtract;

  procedure Lemma_Can_Subtract_Overlap_Pts(p: point_2d; e, s: segment_2d)
  is null;

  --  procedure Lemma_Transitive_Vertex_Inclusion(p1: polygon_2d; p2: polygon_2d;
  --                                              n_p1 : polygon_2d;
  --                                              n_p2 : polygon_2d;
  --                                              vertices : uniq_vertex_list; merged : polygon_2d)
  --  is null;

end Lemmas;
