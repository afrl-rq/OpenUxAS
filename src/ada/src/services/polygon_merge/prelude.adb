package body prelude with SPARK_Mode is
   
  procedure Lemma_both_sides_times_pos_le1_imp(x, y: in Float; nnw: in nn_float)
  is begin
    pragma Assert(x <= y);
    pragma Assert(nnw >= 0.0);
    -- FAN Argument
    --  Believing
    --    The following Assume is true
    --  because
    --    The PVS lemma both_sides_times_pos_le1_imp has been proven true for reals
    --    Floating point arithmetic will not change the truth value of the predicate
    --  premises
    --    x <= y AND nnw >= 0
    --  end
    pragma Assume(x * nnw <= y * nnw);
  end Lemma_both_sides_times_pos_le1_imp;

  procedure Lemma_both_sides_times_neg_le1_imp(x, y: in Float; npw: in np_float)
  is begin
    pragma Assert(y <= x);
    pragma Assert(npw <= 0.0);
    -- FAN Argument
    --  Believing
    --    The following Assume is true
    --  because
    --    The PVS lemma both_sides_times_neg_le1_imp has been proven true for reals
    --    Floating point arithmetic will not change the truth value of the predicate
    --  premises
    --    y <= x AND npw <= 0
    --  end
    pragma Assume(x * npw <= y * npw);
  end Lemma_both_sides_times_neg_le1_imp;

end prelude;
