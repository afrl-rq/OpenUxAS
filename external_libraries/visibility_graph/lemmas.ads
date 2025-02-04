with floats;

package Lemmas with SPARK_Mode,
  Ghost is -- all content is ghost
  use floats;

  procedure Lemma_NonNeg_Mult_By_O_to_1_LE_Orig(x, t: Float) with
    Global => null,
    Pre => x >= 0.0 and 0.0 <= t and t <= 1.0,
    Post => t * x <= x;

  procedure Lemma_Neg_Mult_By_O_to_1_GE_Orig(x, t: Float) with
    Global => null,
    Pre => x < 0.0 and 0.0 <= t and t <= 1.0,
    Post => x <= t * x;

  procedure Lemma_Between_Two_Values_With_Sum(x1, x2, t: Float) with
    Global => null,
    Pre => add_constraint(x1, x2) and (0.0 <= t and t <= 1.0),
    Post => multiply_constraint(t, x2) and then add_constraint(x1, t * x2) and then
    ((x1 <= x1 + t * x2 and x1 + t * x2 <= x1 + x2) or
     (x1 + x2 <= x1 + t * x2 and x1 + t * x2 <= x1));

end Lemmas;
