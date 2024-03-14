-- -----------------------------------------------------------------------------
-- vectors_2d.ads               Dependable Computing
-- Corresponds to logic from vectors@vectors_2D.pvs and vectors_2d.pvs
-- -----------------------------------------------------------------------------
-- Declaration of types, constants, and common functions on 2D vectors.
with prelude;
with floats;
with Ada.Numerics.Elementary_Functions;
package vectors_2d with SPARK_Mode is
  use prelude;
  use floats;
  -----------------------------------------------------------------------------
  -- begin retrenchment section
  -----------------------------------------------------------------------------
  -- To control float overflow error, define a subrange of float over which points
  -- for our polygons can be defined. Note that this does not completely
  -- eliminate overflow error, though we don't understand why not.
  subtype vector_float_type is Float range -2.0**52 .. 2.0**52;

  -- We want the length of a 2d vector to be easy to calculate
  -- I.e., we want x^2 + y^2 <= vector_float_type'Last
  function vector_2d_constraint(x: vector_float_type;
                                y: vector_float_type) return Boolean
  is
    (multiply_constraint(x, x) and then multiply_constraint(y, y) and then
       add_constraint(x * x, y * y))
      with Ghost, Post => True;

   -- This seems like a useless function, but it's for helping the prover to know this
   -- fact in other places where it's needed in order to speed up proof that floats
   -- do not overflow
   function in_vector_float_range(a: Float) return Boolean is
      (vector_float_type'First <= a and a <= vector_float_type'Last);

  -- Because of absorption, it's possible for a <= vector_float_type'Last - b but
  -- not b <= vector_float_type'Last - a or vice-versa, e.g., if
  -- b = vector_float_type'Last and a = 1.0. However, if at least one of these
  -- two conditions is true, then a + b <= vector_float_type'Last, which is what
  -- we really care about. Similar logic applies to vector_float_type'First.
  function can_add(a: Float; b: Float) return boolean
  is
    (add_constraint(a, b) and then
     in_vector_float_range(a + b))
      with Ghost;

    --  ((((a >= 0.0 and then b >= 0.0) and then
    --       ((a <= vector_float_type'Last - b) or else
    --          (b <= vector_float_type'Last - a))) or else
    --     (a >= 0.0 and then b < 0.0) or else
    --     (a < 0.0 and then b >= 0.0) or else
    --     ((a < 0.0 and then b < 0.0) and then
    --          ((a >= vector_float_type'First - b) or else
    --             (b >= vector_float_type'First - a)))) and then
    --     ((a + b) >= vector_float_type'First) and then
    --       ((a + b) <= vector_float_type'Last))
    --      with Post => True;

  -- SPARK doesn't allow us to use Float'Epsilon. So ... We'll define our
  -- own.
  --
  -- TODO: make this smaller?
  vector_float_eps: constant vector_float_type := 0.000_000_000_1;
  -- TODO: Make lemma that vector_float_eps^2 > 0

  -----------------------------------------------------------------------------
  -- end retrenchment section
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- begin vectors@vectors_2D.pvs types
  -----------------------------------------------------------------------------

  -- A 2D vector, represented by an x and a y coordinate.
  --   Vect2 : TYPE = [#  x, y: real  #]
  -- combined with a retrenchment predicate
  type Vect2 is record
    x: vector_float_type;
    y: vector_float_type;
  end record with
    Ghost_Predicate => vector_2d_constraint(Vect2.x, Vect2.y);

  -----------------------------------------------------------------------------
  -- end vectors@vectors_2D.pvs types
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- begin retrenchment section 2
  -----------------------------------------------------------------------------
  -- Predicate that p2 can be added to p1
  function can_add(p1: Vect2; p2: Vect2) return Boolean is
    (can_add(p1.x, p2.x) and then can_add(p1.y, p2.y) and then
     vector_2d_constraint(p1.x + p2.x, p1.y + p2.y))
      with Ghost, Post => True;

  -- Predicate that p2 can be subtracted from p1
  function can_subtract(p1: Vect2; p2: Vect2) return Boolean is
    (can_add(p1.x, -p2.x) and then can_add(p1.y, -p2.y) and then
     vector_2d_constraint(p1.x - p2.x, p1.y - p2.y))
      with Ghost, Post => True; -- Per Johannes Kanig helps prover to use expression function as post-condition

  function can_multiply(a: vector_float_type; v: Vect2) return Boolean is
    (in_vector_float_range(a * v.x) and then in_vector_float_range(a * v.y) and then
     vector_2d_constraint(a * v.x, a * v.y))
      with Ghost;

  -- An epsilon vector.
  eps_vector: constant Vect2 := Vect2'(x => vector_float_eps,
                                       y => vector_float_eps);
  -- TODO: Add lemma that norm(eps_vector) > 0
  -----------------------------------------------------------------------------
  -- end retrenchment section 2
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- begin vectors@vectors_2D.pvs functionality
  -----------------------------------------------------------------------------

  -- A zero vector.
  --   zero: Vector = (0,0)
  zero: constant Vect2 := (x => 0.0, y => 0.0);

  --  Vector    : TYPE = Vect2
  --  u,v,w,z   : VAR Vector
  --...
  -- PVS:
  --   +(u,v): Vector = (u`x + v`x, u`y + v`y) ;
  -- Define element-wise addition for 2D vectors.
  function "+" (u: Vect2;
                v: Vect2) return Vect2 is
    ((x => u.x + v.x,
      y => u.y + v.y))
    with Pre => can_add(u, v);

  -- PVS:
  --   -(u,v): Vector = (u`x - v`x, u`y - v`y) ;
  -- Define element-wise subtraction for 2D vectors.
  function "-" (u: Vect2;
                v: Vect2) return Vect2 is
    ((x => u.x - v.x,
      y => u.y - v.y))
    with Pre => can_subtract(u, v);

   --  -(v)  : Vector = (-v`x, -v`y) ;
  function "-" (v: Vect2) return Vect2 is
    ((x => -v.x, y => -v.y));

  -- PVS:
  --   *(u,v): real  =  u`x * v`x + u`y * v`y ;
  -- Define dot product for 2D vectors.
  function "*" (u: Vect2;
                v: Vect2) return Float is
    (u.x * v.x + u.y * v.y);

  -----------------------------------------------------------------------------
  -- begin retrenchment section 3
  -----------------------------------------------------------------------------
  function non_zeroish(v: Vect2) return Boolean is
    (v * v > 0.0); -- TODO: Check to see if Ghost
  -----------------------------------------------------------------------------
  -- end retrenchment section 3
  -----------------------------------------------------------------------------

  --  *(a,v): Vector = (a * v`x, a * v`y) ;
  function "*" (a: vector_float_type;
                v: Vect2) return Vect2 is
    ((x => a * v.x, y => a * v.y))
      with Pre => can_multiply(a, v);

  --  sqv(v): nnreal = v*v
  function sqv(v: Vect2) return nn_float is
    (v * v)
    with Post =>
      (sqv'Result >= 0.0 and -- redundant
         (if (non_zeroish(v)) then sqv'Result > 0.0));

  --  norm(v): nnreal = sqrt(sqv(v))
  function norm(v: Vect2) return nn_float is
    (Ada.Numerics.Elementary_Functions.Sqrt(sqv(v)))
    with
      Pre => vector_2d_constraint(v.x, v.y), -- redundant
      Post =>
        (if (non_zeroish(v)) then norm'Result > 0.0);

  --  normalized?(v): MACRO bool =
  --    norm(v) = 1
  function is_normalized(v: Vect2) return Boolean is
    (norm(v) = 1.0);

  --  ^(nzv)         : Normalized = (1/norm(nzv))*nzv
  --  normalize(nzv) : MACRO Normalized = ^(nzv)
  function normalize(nzv: Vect2) return Vect2 is
    ((1.0/norm(nzv))*nzv)
    with Pre =>
      non_zeroish(nzv) and then norm(nzv) > vector_float_eps and then
    can_multiply(1.0/norm(nzv), nzv);
  -- TODO: The last part of the above precondition (can_multiply) should not be
  -- required, as it should always hold from the prior preconditions
  -- Can't prove the following due to floating-point rounding
      --  Post =>
      --    is_normalized(normalize'Result);

  -----------------------------------------------------------------------------
  -- end vectors@vectors_2D.pvs functionality
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- begin vectors_2d.pvs types
  -----------------------------------------------------------------------------

  -- PVS:
  --   vector_2d: TYPE = Vect2;
  -- Alias a Vect2 to be a 2D vector.
  subtype vector_2d is Vect2;
  -- PVS:
  --   point_2d: TYPE = Vect2;
  -- Alias a Vect2 to be a 2D point.
  subtype point_2d is Vect2;

  -----------------------------------------------------------------------------
  -- end vectors_2d.pvs types
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- begin retrenchment section 4
  -----------------------------------------------------------------------------
  function Real_Eq(p1, p2 : point_2d) return Boolean with
    Global => null,
    Ghost,
    Import,
    Annotate => (GNATprove, Logical_Equal);
  -----------------------------------------------------------------------------
  -- end retrenchment section 4
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- begin vectors_2d.pvs functionality
  -----------------------------------------------------------------------------

  -- A zero vector.
  -- PVS:
  --   zero_vector: vector_2d = zero;
  --   zero_point: point_2d = zero;
  zero_vector: constant vector_2d := zero;
  zero_point: constant point_2d := zero;

  -----------------------------------------------------------------------------
  -- end vectors_2d.pvs functionality
  -----------------------------------------------------------------------------

end vectors_2d;
