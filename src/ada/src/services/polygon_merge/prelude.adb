package body prelude with SPARK_Mode is
   
   procedure both_sides_times_pos_le1_imp(x, y: in Float; nnw: in nn_float)
   is begin
      -- WARNING: inserting this as an unproven axiom!
      pragma Assume(x * nnw <= y * nnw);
   end both_sides_times_pos_le1_imp;

   procedure both_sides_times_neg_le1_imp(x, y: in Float; npw: in np_float)
   is begin
      -- WARNING: inserting this as an unproven axiom!
      pragma Assume(x * npw <= y * npw);
   end both_sides_times_neg_le1_imp;

end prelude;
