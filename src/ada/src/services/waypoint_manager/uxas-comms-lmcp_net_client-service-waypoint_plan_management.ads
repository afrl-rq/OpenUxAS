with DOM.Core;

with Waypoint_Plan_Manager;               use Waypoint_Plan_Manager;
with Waypoint_Plan_Manager_Communication; use Waypoint_Plan_Manager_Communication;

with AFRL.CMASI.Enumerations; use AFRL.CMASI.Enumerations;
with AFRL.CMASI.MissionCommand; use AFRL.CMASI.MissionCommand;

with Common; use Common;

package UxAS.Comms.LMCP_Net_Client.Service.Waypoint_Plan_Management is

   type Waypoint_Plan_Manager_Service is new Service_Base with private;

   type Waypoint_Plan_Manager_Service_Ref is access all Waypoint_Plan_Manager_Service;

   Type_Name : constant String := "WaypointPlanManagerService";

   Directory_Name : constant String := "";

   function Registry_Service_Type_Names return Service_Type_Names_List;

   function Create return Any_Service;

private

   type Waypoint_Plan_Manager_Service is new Service_Base with record
      Timer        : Common.Int64 := 0;
      Time_Elapsed : Boolean := False;
      Min_Time_Between_Commands_ms : Common.Int64 := 1000;
      Config  : Waypoint_Plan_Manager_Configuration_Data;
      Mailbox : Waypoint_Plan_Manager_Mailbox;
      State   : Waypoint_Plan_Manager_State;
   end record;

   overriding
   procedure Configure
     (This     : in out Waypoint_Plan_Manager_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean);

   overriding
   procedure Initialize
     (This   : in out Waypoint_Plan_Manager_Service;
      Result : out Boolean);

   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Waypoint_Plan_Manager_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean);

--  TODO: TIMER CALLBACKS
--  this function gets called when the response timer expires
--  void OnResponseTimeout();
--  this function gets called when the tasks involved have not reported initialization in time
--  void OnTasksReadyTimeout();

end UxAS.Comms.LMCP_Net_Client.Service.Waypoint_Plan_Management;
