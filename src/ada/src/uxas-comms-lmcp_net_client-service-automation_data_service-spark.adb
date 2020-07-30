
with Ada.Text_IO; use Ada.Text_IO;
with Bounded_Dynamic_Strings;
with afrl.cmasi.KeyValuePair;       use afrl.cmasi.KeyValuePair;
with Ada.Strings.Unbounded;
with afrl.cmasi.AutomationResponse; use afrl.cmasi.AutomationResponse;
with afrl.cmasi.AutomationResponse.SPARK_Boundary; use afrl.cmasi.AutomationResponse.SPARK_Boundary;

package body UxAS.Comms.LMCP_Net_Client.Service.Automation_Data_Service.SPARK with SPARK_Mode is


   ------------------
   -- Empty_Vector --
   ------------------

   function Empty_Vector return Int64_Vect is
   begin
      pragma Warnings (Off);
      return V : Int64_Vect do
         null;
      end return;
      pragma Warnings (On);
   end Empty_Vector;

   function Vehicle_Id_Seen_Before(This : Automation_Data_Service; Id : Int64) return Boolean is
     (Int64_Sets.Contains(This.Configs.AutomationIds, Id));


   procedure Handle_MissionCommand
     (This    : in Automation_Data_Service;
      Command : My_Object_Any;
      KnownVehicleId : out Boolean)
   is
   begin
      if Vehicle_Id_Seen_Before(This, MissionCommand(Deref(Command)).getVehicleID) then
         Put_Line("<>AutomationDataService**: Got mission command with ID " &
                    MissionCommand(Deref(Command)).getVehicleID'Image);
         KnownVehicleId := True;
      else
         KnownVehicleId := False;
      end if;
   end Handle_MissionCommand;


end UxAS.Comms.LMCP_Net_Client.Service.Automation_Data_Service.SPARK;
