with UxAS.Comms.LMCP_Net_Client.Service.Example_Spark_Service; use UxAS.Comms.LMCP_Net_Client.Service.Example_Spark_Service;

with AFRL.CMASI.lmcpTask.SPARK_Boundary;           use AFRL.CMASI.lmcpTask.SPARK_Boundary;
with AFRL.CMASI.MissionCommand;                    use AFRL.CMASI.MissionCommand;
with AFRL.CMASI.AutomationResponse;                use AFRL.CMASI.AutomationResponse;
with AFRL.CMASI.MissionCommand.SPARK_Boundary;     use AFRL.CMASI.MissionCommand.SPARK_Boundary;
with AFRL.CMASI.AutomationResponse.SPARK_Boundary; use AFRL.CMASI.AutomationResponse.SPARK_Boundary;

with AFRL.CMASI.AutomationRequest;                use AFRL.CMASI.AutomationRequest;
with AFRL.CMASI.AutomationRequest.SPARK_Boundary; use AFRL.CMASI.AutomationRequest.SPARK_Boundary;
with AVTAS.LMCP.Object.SPARK_Boundary;            use AVTAS.LMCP.Object.SPARK_Boundary;
with Ada.Containers;                              use Ada.Containers;

private
package UxAS.Comms.LMCP_Net_Client.Service.Example_Spark_Service.SPARK with SPARK_Mode is
   use all type Int64_Set;

   --------------------------------------
   -- Functions for annotation purpose --
   --------------------------------------

   function Recognized_VehicleId_From_Previous_AutomationResponse
     (This : Example_Spark_Service; Id : Int64) return Boolean is
     (Int64_Sets.Contains (This.Configs.AutomationIds, Id));

   -------------------------------------
   -- Regular Service Functionalities --
   -------------------------------------

   procedure Handle_MissionCommand
     (This          : Example_Spark_Service;
      Command       : My_Object_Any;
      Recognized_Id : out Boolean)
   with
     Pre => Deref (Command) in MissionCommand,
     Post =>
       (if Recognized_VehicleId_From_Previous_AutomationResponse
             (This, MissionCommand (Deref (Command)).getVehicleID)
        then Recognized_Id else not Recognized_Id);

end UxAS.Comms.LMCP_Net_Client.Service.Example_Spark_Service.SPARK;
