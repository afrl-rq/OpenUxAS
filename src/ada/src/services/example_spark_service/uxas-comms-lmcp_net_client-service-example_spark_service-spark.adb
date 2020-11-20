
with Ada.Text_IO;                                  use Ada.Text_IO;
with Bounded_Dynamic_Strings;
with AFRL.CMASI.KeyValuePair;                      use AFRL.CMASI.KeyValuePair;
with Ada.Strings.Unbounded;
with AFRL.CMASI.AutomationResponse;                use AFRL.CMASI.AutomationResponse;
with AFRL.CMASI.AutomationResponse.SPARK_Boundary; use AFRL.CMASI.AutomationResponse.SPARK_Boundary;

package body UxAS.Comms.LMCP_Net_Client.Service.Example_Spark_Service.SPARK with SPARK_Mode is

   ---------------------------------
   -- Handle_MissionCommand --
   ---------------------------------

   procedure Handle_MissionCommand
     (This          : Example_Spark_Service;
      Command       : My_Object_Any;
      Recognized_Id : out Boolean)
   is
   begin
      if Recognized_VehicleId_From_Previous_AutomationResponse
           (This, MissionCommand (Deref (Command)).getVehicleID)
      then
         Put_Line ("**ExampleSparkService**: Got MissionCommand with VehicleID " &
                   MissionCommand (Deref (Command)).getVehicleID'Image &
                   " previously seen in an AutomationResponse");
         Recognized_Id := True;
      else
         Recognized_Id := False;
      end if;
   end Handle_MissionCommand;

end UxAS.Comms.LMCP_Net_Client.Service.Example_Spark_Service.SPARK;
