with vectors_2d;

package Vector_Lemmas with SPARK_Mode,
    Ghost is -- all content is ghost
  use vectors_2d;

  procedure Lemma_Vector_2d_Constraint_Even(x: vector_float_type; y: vector_float_type) with
    Global => null,
    Pre => vector_2d_constraint(x, y),
    Post => vector_2d_constraint(-x, -y);

  procedure Lemma_Can_Add_Symmetric(a: vector_float_type; b: vector_float_type) with
    Global => null,
    Pre => can_add(a, b),
    Post => can_add(b, a);

  procedure Lemma_Can_Add_Even(a: vector_float_type; b: vector_float_type) with
    Global => null,
    Pre => can_add(a, b),
    Post => can_add(-a, -b);

  procedure Lemma_Can_Always_Add_Zero(a: vector_float_type) with
    Global => null,
    Pre => true,
    Post => can_add(a, 0.0);

  procedure Lemma_Can_Always_Add_Zero_Vec(v: vector_2d) with
    Global => null,
    Pre => true,
    Post => can_add(v, zero);

  procedure Lemma_Can_Subtract_Degenerate(v: vector_2d) with
    Global => null,
    Pre => true,
    Post => can_subtract(v, v);

  procedure Lemma_Can_Subtract_Symmetric(p1: vector_2d; p2: vector_2d) with
    Global => null,
    Pre => can_subtract(p1, p2),
    Post => can_subtract(p2, p1);

end Vector_Lemmas;
