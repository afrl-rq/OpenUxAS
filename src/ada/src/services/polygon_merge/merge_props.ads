-- -----------------------------------------------------------------------------
-- merge_props.ads              Dependable Computing
-- Corresponds to logic from merge_props.pvs
-- -----------------------------------------------------------------------------
with polygons_2d;
with vertex_list;
with injection_props;
with polygon_merge;

package merge_props with SPARK_Mode is
  use polygons_2d;
  use vertex_list;
  use injection_props;
  use polygon_merge;

  function simple_merged_polygon(A, B: simple_polygon_2d) return simple_polygon_2d is
    (bounded_vertex_list_to_polygon(merge_seq(A, B))) with
    Pre => (merge_seq_pre(A, B)) and then
    (uniq_vertex_list_pred(merge_seq(A, B).num_vertices, merge_seq(A, B).vertices)) and then
    (polygon_2d_constraint(merge_seq(A, B)));

end merge_props;
