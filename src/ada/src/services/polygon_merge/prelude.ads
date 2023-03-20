-- -----------------------------------------------------------------------------
-- prelude.ads               Dependable Computing
-- Based on PVS prelude (and nasalib/reals/sq.pvs)
-- -----------------------------------------------------------------------------
with floats;

package prelude with SPARK_Mode is
   use floats;

-- From integers theory in prelude.pvs
--   i, j: VAR int
-- ...
--   upfrom(i): NONEMPTY_TYPE = {s: int | s >= i} CONTAINING i
--   above(i):  NONEMPTY_TYPE = {s: int | s > i} CONTAINING i + 1

-- cSpell: enableCompoundWords
-- From naturalnumbers theory in prelude.pvs
--   naturalnumber: TYPE = nonneg_int
--   nat: NONEMPTY_TYPE = naturalnumber
-- ...
--   i, j, k: VAR nat
-- ...
--   upto(i):   NONEMPTY_TYPE = {s: nat | s <= i} CONTAINING i
--   below(i):  TYPE = {s: nat | s < i}  % may be empty
   subtype nat is integer range 0 .. integer'Last;
--   subtype below(i: nat) is nat range 0 .. (i - 1);
   function below(upper: nat; i: nat) return boolean is (i < upper);

-- From reals theory in prelude.pvs
--   nonzero_real: NONEMPTY_TYPE = {r: real | r /= 0} CONTAINING 1
--   nzreal: NONEMPTY_TYPE = nonzero_real
   subtype nz_float is Float
      with Predicate => (nz_float /= 0.0);

-- From real_types theory in prelude.pvs
--   nonneg_real: NONEMPTY_TYPE = {x: real        | x >= 0} CONTAINING 0
--   nonpos_real: NONEMPTY_TYPE = {x: real        | x <= 0} CONTAINING 0
--   posreal:     NONEMPTY_TYPE = {x: nonneg_real | x > 0}  CONTAINING 1
--   negreal:     NONEMPTY_TYPE = {x: nonpos_real | x < 0}  CONTAINING -1
--   nnreal: TYPE = nonneg_real
--   npreal: TYPE = nonpos_real
  subtype nn_float is Float range 0.0 .. Float'Last;
  subtype pos_float is Float range Float'Small .. Float'Last;
  subtype np_float is Float range -Float'Last .. 0.0;
  subtype neg_float is Float range -Float'Last .. -Float'Small;
  subtype ui_float is Float range 0.0 .. 1.0; -- unit interval float

-- From real_defs theory in prelude.pvs
--   max(m, n): {p: real | p >= m AND p >= n}
--     = IF m < n THEN n ELSE m ENDIF
   function max(m, n: in float) return float is
     (if (m < n) then n else m) with
       Post => max'Result >= m and max'Result >= n;

-- From real_defs theory in prelude.pvs
--   min(m, n): {p: real | p <= m AND p <= n}
--     = IF m > n THEN n ELSE m ENDIF
   function min(m, n: in float) return float is
     (if (m > n) then n else m) with
       Post => min'Result <= m and min'Result <= n;

-- From real_props theory in prelude.pvs
--   both_sides_times_pos_le1_imp: LEMMA x <= y IMPLIES x * nnw <= y * nnw
   procedure both_sides_times_pos_le1_imp(x, y: in Float; nnw: in nn_float) with
     Ghost,
     Global => null,
     Pre => x <= y and multiply_constraint(x, nnw) and multiply_constraint(y, nnw),
     Post => x * nnw <= y * nnw;

-- From real_props theory in prelude.pvs
--   both_sides_times_neg_le1_imp: LEMMA y <= x IMPLIES x * npw <= y * npw
   procedure both_sides_times_neg_le1_imp(x, y: in Float; npw: in np_float) with
     Ghost,
     Global => null,
     Pre => y <= x and multiply_constraint(x, npw) and multiply_constraint(y, npw),
     Post => x * npw <= y * npw;

-- From nasalib/reals/sq.pvs
--   sq(a): nonneg_real = a*a
   function sq(a: Float) return nn_float is (a*a)
   with Pre => (square_constraint(a));

end prelude;
