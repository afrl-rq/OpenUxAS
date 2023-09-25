-- -----------------------------------------------------------------------------
-- vectors_list.ads               Dependable Computing
-- Corresponds to logic from vectors_list.pvs
-- -----------------------------------------------------------------------------
with vectors_2d;

package vertex_list with SPARK_Mode is
  use vectors_2d;

  -----------------------------------------------------------------------------
  -- begin retrenchment section
  -----------------------------------------------------------------------------
  -- Retrenchment: Can only have vertex lists of size 100
  MAX_NUM_VERTICES: constant Natural := 100;

  -- Represent the vertices of a polygon as an array of unspecified range
  -- of the Positives containing 2D points.
  -- This takes the place of the PVS function mapping
  --   below(num_vertices) -> point_2d
  -- where num_vertices can be arbitrarily large
   type bounded_vertex_array is array (0 .. (MAX_NUM_VERTICES-1)) of point_2d;
   type bounded_vertex_list is record
      num_vertices: Natural range 0 .. MAX_NUM_VERTICES;
      vertices: bounded_vertex_array;
   end record
     with
       Ghost_Predicate =>
         ((num_vertices = 0) or else
            (for all i in 0 .. num_vertices-1 =>
               (for all j in 0 .. num_vertices - 1 =>
                 can_subtract(bounded_vertex_list.vertices(i),
                              bounded_vertex_list.vertices(j)))));
  -----------------------------------------------------------------------------
  -- end retrenchment section
  -----------------------------------------------------------------------------

  -- From vertex_list.pvs:
  --   uniq_vertex_list?(num_vertices: nonneg_int)
  --                    (vertices: [below(num_vertices) -> point_2d]): bool =
  --     FORALL(i: below(num_vertices), j: {n: below(num_vertices) | n /= i}):
  --       vertices(i) /= vertices(j);
  function uniq_vertex_list_pred(num_vertices: Natural;
                                 vertices: bounded_vertex_array) return Boolean
  is
    ((num_vertices = 0) or else
         (for all i in 0 .. num_vertices-2 =>
              (for all j in i+1 .. num_vertices-1 =>
                      vertices(i) /= vertices(j))))
         with
   Pre => (num_vertices <= MAX_NUM_VERTICES);

  -- From vertex_list.pvs:
  --   uniq_vertex_list(num_vertices: nonneg_int): NONEMPTY_TYPE =
  --     (uniq_vertex_list?(num_vertices));
  -- Represent the vertices of a polygon as an array of unspecified range
  -- of the Positives containing 2D points.
  -- Note that subtypes do not allow discriminants, so uses of this subtype
  -- will have to change
  subtype uniq_vertex_list is bounded_vertex_list with
    Predicate =>
     uniq_vertex_list_pred(uniq_vertex_list.num_vertices, 
                           uniq_vertex_list.vertices);

  empty_seq: constant uniq_vertex_list := (num_vertices => 0,
                                           vertices => (others => zero));

  -- From vertex_list.pvs:
  --  singleton_seq(p: point_2d): {uvs: uniq_vertex_seq | uvs`length = 1} =
  --  (# length := 1, seq := LAMBDA(i: below(1)): p #);
  function singleton_seq(p: point_2d) return uniq_vertex_list
  is
    (num_vertices => 1, vertices => (0 => p, others => zero));

end vertex_list;
