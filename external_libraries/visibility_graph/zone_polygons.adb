package body zone_polygons with SPARK_Mode is

   function zone_polygon_seq_to_polygon_seq(zps: FSFZ.FS.Finseq) return FSFP.FS.Finseq is
      Result: FSFP.FS.Finseq := FSFP.FS.Empty_Seq;
   begin
      for i in 0 .. zps.Length - 1 loop
         Result := FSFP.Append(Result, zps.Seq(i).polygon);
      end loop;
      return Result;
   end zone_polygon_seq_to_polygon_seq;

   function eps_polygon_seq_to_zone_polygon_seq(eps: eps_type; ps: FSFP.FS.Finseq;
                                                model: zone_polygon) return FSFZ.FS.Finseq is
      Result: FSFZ.FS.Finseq := FSFZ.FS.Empty_Seq;
   begin
      for i in 0 .. ps.Length - 1 loop
         Result := FSFZ.Append(Result, (eps => eps, polygon => ps.Seq(i),
                                        is_keep_in => model.is_keep_in, is_original => False));
      end loop;
      return Result;
   end eps_polygon_seq_to_zone_polygon_seq;

end zone_polygons;
