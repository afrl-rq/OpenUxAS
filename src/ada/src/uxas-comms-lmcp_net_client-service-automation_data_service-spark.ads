with UxAS.Comms.LMCP_Net_Client.Service.Automation_Data_Service; use UxAS.Comms.LMCP_Net_Client.Service.Automation_Data_Service;

with afrl.cmasi.lmcpTask.SPARK_Boundary; use afrl.cmasi.lmcpTask.SPARK_Boundary;
with afrl.cmasi.MissionCommand; use afrl.cmasi.MissionCommand;
with afrl.cmasi.AutomationResponse; use afrl.cmasi.AutomationResponse;
with afrl.cmasi.MissionCommand.SPARK_Boundary; use afrl.cmasi.MissionCommand.SPARK_Boundary;
with afrl.cmasi.AutomationResponse.SPARK_Boundary; use afrl.cmasi.AutomationResponse.SPARK_Boundary;


with afrl.impact.ImpactAutomationRequest; use afrl.impact.ImpactAutomationRequest;
with afrl.impact.ImpactAutomationRequest.SPARK_Boundary; use afrl.impact.ImpactAutomationRequest.SPARK_Boundary;
with uxas.messages.lmcptask.TaskAutomationRequest; use uxas.messages.lmcptask.TaskAutomationRequest;
with uxas.messages.lmcptask.TaskAutomationRequest.SPARK_Boundary; use uxas.messages.lmcptask.TaskAutomationRequest.SPARK_Boundary;
with afrl.cmasi.AutomationRequest; use afrl.cmasi.AutomationRequest;
with afrl.cmasi.AutomationRequest.SPARK_Boundary; use afrl.cmasi.AutomationRequest.SPARK_Boundary;
with avtas.lmcp.object.SPARK_Boundary; use avtas.lmcp.object.SPARK_Boundary;
with Ada.Containers; use Ada.Containers;

private
package UxAS.Comms.LMCP_Net_Client.Service.Automation_Data_Service.SPARK with SPARK_Mode is
   use all type Int64_Vect;
   use all type Int64_Set;

   --------------------------------------
   -- Functions for annotation purpose --
   --------------------------------------

   function All_Elements_In (V : Int64_Vect; S : Int64_Set) return Boolean is
     (for all J in Int64_Vects.First_Index (V) .. Int64_Vects.Last_Index (V)
      => Int64_Sets.Contains (S, Int64_Vects.Element (V, J)))
   with Ghost;

   function Empty_Vector return Int64_Vect with Ghost,
     Post => Int64_Vects.Is_Empty (Empty_Vector'Result);

   function Vehicle_Id_Seen_Before(This : Automation_Data_Service; Id : Int64)
      return Boolean;

   -------------------------------------
   -- Regular Service Functionalities --
   -------------------------------------

   procedure Handle_MissionCommand
     (This           : in Automation_Data_Service;
      Command        : My_Object_Any;
      KnownVehicleId : out Boolean)
   with Pre => Deref (Command) in MissionCommand,
        Post => (if Vehicle_Id_Seen_Before(This, MissionCommand(Deref(Command)).getVehicleID) then KnownVehicleId else not KnownVehicleId);

end UxAS.Comms.LMCP_Net_Client.Service.Automation_Data_Service.SPARK;
