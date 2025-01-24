-- -----------------------------------------------------------------------------
-- zone_polygons.ads              Dependable Computing
-- Corresponds to logic from zone_polygons.pvs
-- -----------------------------------------------------------------------------
with polygons_2d;
with eps_segments;
with eps_polygons;
with Finite_Sequences;
with Finseq_Fns;
with reverse_polygons;
with vectors_2d;
package zone_polygons with SPARK_Mode is

   use polygons_2d;
   use eps_polygons;
   use eps_segments;
   use vectors_2d;
   use reverse_polygons;

   -- PVS:
   --  zone_polygon: TYPE =
   --    [#
   --      eps: eps_type,
   --      polygon: eps_polygon(eps),
   --      keep_in?: boolean,
   --      original?: boolean
   --     #];
   type zone_polygon is record
      eps: eps_type;
      polygon: simple_polygon_2d;
      is_keep_in: Boolean;
      is_original: Boolean;
   end record
     with
       Ghost_Predicate => is_eps_polygon(zone_polygon.polygon, zone_polygon.eps);

   -- Not in the PVS theory:
   Default_Zone_Polygon: constant zone_polygon :=
     (eps => Default_Eps,
      polygon => Eps_Square,
      is_keep_in => True,
      is_original => True);

   package FSFZ is new Finseq_Fns(T => zone_polygon, Default_Value => Default_Zone_Polygon);
   package FSFP is new Finseq_Fns(T => polygon_2d, Default_Value => Eps_Square);

   -- PVS:
   --  fs_zone_polygons: TYPE =
   --    {fs: finseq[zone_polygon] | length(fs) = 0 OR
   --      (FORALL(i: below(length(fs))): fs`seq(i)`eps = fs`seq(0)`eps)};
   -- Instead of creating a type, use the following predicate
   function is_fs_zone_polygons(fs: FSFZ.FS.Finseq) return Boolean is
     ((fs.Length = 0) or
          (for all i in 0 .. fs.Length =>
                fs.Seq(i).eps = fs.Seq(0).eps));

   -- PVS:
   --  eps(zps: {s: fs_zone_polygons | length(s) > 0}): eps_type = zps`seq(0)`eps;
   function eps(zps: FSFZ.FS.Finseq) return eps_type is
     (zps.Seq(0).eps)
       with
         Pre => zps.Length > 0 and is_fs_zone_polygons(zps);

   -- PVS:
   --  zone_polygon_seq_to_polygon_seq(zps: fs_zone_polygons):
   --      fs_polygons =
   --    (# length := zps`length,
   --       seq := LAMBDA(i: below(zps`length)):
   --                zps`seq(i)`polygon #)
   function zone_polygon_seq_to_polygon_seq(zps: FSFZ.FS.Finseq) return FSFP.FS.Finseq;

   -- PVS:
   --  eps_polygon_seq_to_zone_polygon_seq(eps: eps_type, ps: fs_eps_polygons(eps),
   --      model: {zp: zone_polygon | zp`eps = eps}): fs_zone_polygons =
   --    (# length := ps`length,
   --       seq := LAMBDA(i: below(ps`length)):
   --                (# eps := model`eps,
   --                   polygon := ps`seq(i),
   --                   keep_in? := model`keep_in?,
   --                   original? := false
   --                 #)
   --     #);
   function eps_polygon_seq_to_zone_polygon_seq(eps: eps_type; ps: FSFP.FS.Finseq;
                                                model: zone_polygon) return FSFZ.FS.Finseq
     with
       Pre =>
         (for all I in 0 .. ps.Length - 1 =>
            is_eps_polygon(ps.Seq(I), eps)) and
         model.eps = eps;

   -- PVS:
   --  v(zp: zone_polygon): uniq_vertex_list(num_vertices(zp)) =
   --    zp`polygon`vertices;
   -- NB: In PVS, this is used in curried form, but we cannot do that in SPARK
   --  Ada, so we explicitly add the idx argument.
   function v(zp: zone_polygon; idx: Natural) return point_2d is
     (zp.polygon.vertices(idx))
       with
         Pre => idx < zp.polygon.num_vertices;

   -- PVS:
   --  reverse(polygon: zone_polygon): zone_polygon =
   --   (#
   --     eps := polygon`eps,
   --     polygon := reverse_polygon(polygon`polygon),
   --     keep_in? := polygon`keep_in?,
   --     original? := polygon`original?  %% added 2/13
   --    #);
   function reverse_polygon(polygon: zone_polygon) return zone_polygon is
     ((eps => polygon.eps,
       polygon => reverse_polygon(polygon.polygon),
       is_keep_in => polygon.is_keep_in,
       is_original => polygon.is_original));

end zone_polygons;
