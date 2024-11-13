with floats;

package body Vector_Lemmas with SPARK_mode is
  use floats;

--  pragma Warnings
--    (Off, "postcondition does not check the outcome of calling");

  procedure Lemma_Vector_2d_Constraint_Even(x: vector_float_type; y: vector_float_type)
  is begin
    -- ((abs(x) < 1.0 or else abs(x) <= vector_float_type'Last / abs(x)) and then
    --  (abs(y) < 1.0 or else abs(y) <= vector_float_type'Last / abs(y)) and then
    --  (x * x <= vector_float_type'Last - y * y))
    pragma Assert(add_constraint((-x) * (-x), (-y) * (-y)));
  end Lemma_Vector_2d_Constraint_Even;

  procedure Lemma_Can_Add_Symmetric(a: vector_float_type; b: vector_float_type)
  is null;

  procedure Lemma_Can_Add_Even(a: vector_float_type; b: vector_float_type)
  is null;

  procedure Lemma_Can_Always_Add_Zero(a: vector_float_type)
  is null;

  procedure Lemma_Can_Always_Add_Zero_Vec(v: vector_2d)
  is null;

  procedure Lemma_Can_Subtract_Degenerate(v: vector_2d)
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

end Vector_Lemmas;
