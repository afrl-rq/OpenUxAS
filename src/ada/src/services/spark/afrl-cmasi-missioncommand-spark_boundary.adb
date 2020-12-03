package body AFRL.CMASI.MissionCommand.SPARK_Boundary with SPARK_Mode => Off is

   -------------------
   -- Get_VehicleID --
   -------------------

   function Get_VehicleID
     (Command : MissionCommand) return Int64
         renames getVehicleID;

end AFRL.CMASI.MissionCommand.SPARK_Boundary;
