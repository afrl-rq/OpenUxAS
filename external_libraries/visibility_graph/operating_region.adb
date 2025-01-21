package body operating_region is

   function polygon_set_to_fs_zone_polygons(ps: Polygon_Set) return FSFZ.FS.Finseq is
      Result : FSFZ.FS.Finseq := FSFZ.FS.Empty_Seq;
   begin
      -- Iterate through the unique IDs in the Polygon_Set
      for idx in 0 .. ps.Zones.Length - 1 loop
         Result := FSFZ.Append(Result, ps.Zones.Seq(idx).Zone);
      end loop;

      return Result;
   end Polygon_Set_To_FS_Zone_Polygons;

   function add_polygon(region_polygons: FSFZP.FS.Finseq; id: Unique_ID_Type;
                        new_polygon: zone_polygon) return FSFZP.FS.Finseq is
      New_Zones: FSFZP.FS.Finseq := region_polygons;
      Found: Boolean := False;
   begin
      for idx in 0 .. region_polygons.Length - 1 loop
         pragma Loop_Invariant(New_Zones.Length = region_polygons.Length);
         if New_Zones.Seq(idx).ID = id then
            Found := True;
            -- Update the existing zone
            New_Zones.Seq(idx).Zone := new_polygon;
            exit;
         end if;
      end loop;

      if not Found then
         New_Zones := FSFZP.Append(New_Zones, (ID => id, Zone => new_polygon));
      end if;

      return New_Zones;
   end Add_Polygon;

end operating_region;
