
package body AFRL.CMASI.AutomationResponse.SPARK_Boundary with SPARK_Mode => Off is

   --------------------
   -- Get_WaypointEntity_Set --
   --------------------

   function Get_WaypointEntity_Set
     (Response : AutomationResponse) return Int64_Set
   is
      L : constant Vect_MissionCommand_Acc_Acc := Response.getMissionCommandList;
   begin
      return R : Int64_Set do
         for E of L.all loop
            Int64_Sets.Include (R, E.getVehicleID);
         end loop;
      end return;
   end Get_WaypointEntity_Set;

end AFRL.CMASI.AutomationResponse.SPARK_Boundary;
