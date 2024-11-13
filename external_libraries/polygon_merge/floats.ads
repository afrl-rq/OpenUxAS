-- -----------------------------------------------------------------------------
-- floats.ads               Dependable Computing
-- Useful types and constraints for IEEE floating point numbers
-- -----------------------------------------------------------------------------

-- ISSUE: SPARK Ada uses IEEE floating point numbers and PVS uses unbounded, infinite precision reals
-- RESOLUTION: Retrenchment and rigorous supporting argument
package floats with SPARK_Mode is

   -- Rounding component is used to multiply by float limits prior to checking for safety
   -- This works around odd cases such as when b <= Float'Last / a, but a * b > Float'Last
   ROUNDING_COMPONENT: constant := 0.999;

   -- Two floats with an arbitrary relationship: useful for returning a pair of floats
   type float_pair is record
      val1: float;
      val2: float;
   end record;

   -- This seems like a useless function, but it's for helping the prover to know this
   -- fact in other places where it's needed in order to speed up proof that floats
   -- do not overflow
   function in_float_range(a: Float) return Boolean is
      (Float'First <= a and a <= Float'Last);

   -- Can a be added to b
   -- Notes:
   ---   IEEE 74 Floating point numbers are symmetric
   --    If a or b is NaN, this function will return false
   --    If a or b is +/- inf, this function will return false
   --    Rewrite: In the case of finite real floats, the second term is covered by the first, 
   --       but by representing it directly, it is more easily extracted by symbolic analysis
   --       as stating that all addition of finite numbers are in range if the precondition holds: Restate
  function add_constraint(a, b: Float) return Boolean
  is
    ((
       -- Case 0: either a or b are zero
       (a = 0.0 or else b = 0.0) or else
       -- Case 1: both a and b are positive: a + b is definitely larger than min float, so
     -- verify the sum is less than or equal to max float
       ((a > 0.0 and then b > 0.0) and then (a < Float'Last - b) and then (a + b <= Float'Last)) or else
       -- Case 2 and 3: a and b are of opposite sign: adding won't overflow
       (a > 0.0 and then b < 0.0) or else
       (a < 0.0 and then b > 0.0) or else
     -- Case 4: both a and b are negative: a + b is definitely smaller than max float, so
     -- verify the sum is greater than or equal to than min float  
       ((a < 0.0 and then b < 0.0) and then (a > Float'First - b) and then (a + b >= Float'First))) and then
     in_float_range(a + b))
      with Ghost;
  -- Cannot make Ghost because add_constraint is used in vector_2d_constraint, and vector_2d_constraint
  -- is used as a predicate on the Vect2 type. Evidently, type predicates cannot be Ghost

   -- Can b be subtracted from a
   function subtract_constraint(a, b: Float) return Boolean
   is (add_constraint(a, -b))
   with Ghost;

   -- Can a be inverted
   function invert_constraint(a: Float) return Boolean
   is
      (
       -- First make sure that a /= 0
       (a /= 0.0) and then ((
         -- Case 1: magnitude of a is greater than or equal to one, so inverse won't overflow (will be <= 1)
         (a <= -1.0 or else 1.0 <= a) or else
         -- Case 2: magnitude of a is less than 1, so safely check bounds
         (1.0 <= Float'Last * abs a)) and then
        in_float_range(1.0 / a)))
   with Ghost;

   -- Can a be multiplied by b
   function multiply_constraint(a, b: Float) return Boolean
   is
      ((
        -- Case 1: a is zero
        (a = 0.0) or else
        -- Case 2: magnitude of a is less than or equal to one, so product won't overflow
        (abs a <= 1.0) or else
        -- Case 3: magnitude of a is greater than 1, so safely check bounds
        (abs b <= ROUNDING_COMPONENT * Float'Last / abs a)) and then
        in_float_range(a * b));
   -- with Ghost; Have to remove ghost because predicates don't allow it?

   -- Can a be divided by b
   function divide_constraint(a, b: Float) return Boolean
   is
     (
       -- First make sure that b /= 0
       (b /= 0.0) and then ((
         -- Case 1: magnitude of b is greater than or equal to one, so product won't overflow
         (1.0 <= abs b) or else
         -- Case 2: magnitude of b is less than 1, so safely check bounds
         (abs a <= ROUNDING_COMPONENT * Float'Last * abs b)) and then
        in_float_range(a / b)))
   with Ghost;

   -- Can a be squared
   function square_constraint(a: Float) return Boolean is (multiply_constraint(a, a))
   with Ghost;
 
end floats;
