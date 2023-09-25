-- -----------------------------------------------------------------------------
-- vectors_cross_2d.ads               Dependable Computing
-- Corresponds to logic from vectors_cross_2d.pvs
-- -----------------------------------------------------------------------------
with prelude;
with vectors_2d;

package vectors_cross_2d with SPARK_Mode is
  use prelude;
  use vectors_2d;

  -- PVS:
  --   cross(v1, v2: vector_2d): real =
  --     v1`x * v2`y - v1`y * v2`x;
  -- Define the 2-dimensional cross product for 2D vectors. This is not the
  -- typical cross-product, because it does not result in a new vector.
  function cross(v1: vector_2d;
                 v2: vector_2d) return Float is
    (v1.x * v2.y - v1.y * v2.x);


  -- PVS:
  --   vector_from_point_to_point(p1: point_2d, p2: point_2d): vector_2d =
  --     (p2`x - p1`x, p2`y - p1`y);
  -- Build a vector pointing from p1 to p2.
  function vector_from_point_to_point(p1: point_2d;
                                      p2: point_2d) return vector_2d is
    ((x => p2.x - p1.x, y => p2.y - p1.y)) with
      Pre => can_subtract(p2, p1) and then
      vector_2d_constraint(p2.x - p1.x, p2.y - p1.y);

  -- PVS:
  --   are_vectors_collinear?(v1: vector_2d, v2: vector_2d):
  --       {b: bool | b = (abs(v1 * v2) = norm(v1) * norm(v2))} =
  --     cross(v1, v2) = 0;
  -- Retrenched to allow cross products very close to zero or
  -- only approximately collinear.
  -- Return true if the given vectors are colinear.
  function are_vectors_collinear(v1: vector_2d;
                                 v2: vector_2d) return Boolean is
    (abs(cross(v1, v2)) <= vector_float_eps);

end vectors_cross_2d;
