
package body afrl.cmasi.MissionCommand.SPARK_Boundary with SPARK_Mode => Off is

   -------------------------
   -- Get_VehicleID --
   -------------------------

   function Get_VehicleID
     (Command : MissionCommand) return Int64
         renames getVehicleID;


end afrl.cmasi.MissionCommand.SPARK_Boundary;
