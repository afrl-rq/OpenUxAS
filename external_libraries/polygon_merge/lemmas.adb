with prelude;

package body Lemmas with SPARK_mode is

  use prelude;

--  pragma Warnings
--    (Off, "postcondition does not check the outcome of calling");

  procedure Lemma_NonNeg_Mult_By_O_to_1_LE_Orig(x: Float; t: Float)
  is begin
    Lemma_both_sides_times_pos_le1_imp(t, 1.0, x);
  end Lemma_NonNeg_Mult_By_O_to_1_LE_Orig;

  procedure Lemma_Neg_Mult_By_O_to_1_GE_Orig(x: Float; t: Float)
  is begin
    Lemma_both_sides_times_neg_le1_imp(1.0, t, x);
  end Lemma_Neg_Mult_By_O_to_1_GE_Orig;

  procedure Lemma_Between_Two_Values_With_Sum(x1, x2, t: Float)
  is begin
    pragma Assert(multiply_constraint(t, x2));
    pragma Assert(add_constraint(x1, x2)); -- From precondition
    if x2 >= 0.0 then
      pragma Assert(t * x2 <= x2);
      pragma Assert(add_constraint(x1, t * x2));
    else
      pragma Assert(t * x2 >= x2);
      pragma Assert(add_constraint(x1, t * x2));
    end if;
    pragma Assert(add_constraint(x1, t * x2));
    pragma Assert((x1 <= x1 + t * x2 and x1 + t * x2 <= x1 + x2) or
                    (x1 + x2 <= x1 + t * x2 and x1 + t * x2 <= x1));
  end Lemma_Between_Two_Values_With_Sum;

end Lemmas;
