--  see OpenUxAS\src\Services\WaypointPlanManagerService.h

with DOM.Core;

with Waypoint_Plan_Manager;               use Waypoint_Plan_Manager;
with Waypoint_Plan_Manager_Communication; use Waypoint_Plan_Manager_Communication;

with AFRL.CMASI.Enumerations; use AFRL.CMASI.Enumerations;

with Common; use Common;

package UxAS.Comms.LMCP_Net_Client.Service.Waypoint_Plan_Management is

   type Waypoint_Plan_Manager_Service is new Service_Base with private;

   type Waypoint_Plan_Manager_Service_Ref is access all Waypoint_Plan_Manager_Service;

   Type_Name : constant String := "WaypointPlanManagerService";

   Directory_Name : constant String := "";

   --  static const std::vector<std::string>
   --  s_registryServiceTypeNames()
   function Registry_Service_Type_Names return Service_Type_Names_List;

   --  static ServiceBase*
   --  create()
   function Create return Any_Service;

private

   --  static
   --  ServiceBase::CreationRegistrar<AutomationRequestValidatorService> s_registrar;
   --  see the package body executable part

   type Waypoint_Plan_Manager_Service is new Service_Base with record

      WPM_Timer : Common.Int64 := 0;
      WPM_Timer_Triggered : Boolean := False;

      -- ID of the vehicle that this manager is working for
      VehicleID : Common.Int64 := -1;

      -- Radius to use for loiters that are added by the waypoint manager
      LoiterRadiusDefault : Common.Real64 := 200.0;
      -- Add an infinite loiter to the end of each waypoint segment served
      -- IsAddLoiterToEndOfSegments : Boolean := False;
      -- Add an infinite loiter to the last waypoint in the mission
      -- IsAddLoiterToEndOfMission : Boolean := False;
      -- Use the index of the first waypoint with an associated task
      -- as the last waypoint in the mission's "NextWaypoint"
      -- IsLoopBackToFirstTask : Boolean := False;
      -- Set the speed of the last waypoint in the mission to 0.
      -- Used for vehicles (ground, surface) that should stop at end of plan.
      -- IsSetLastWaypointSpeedTo0 : Boolean := False;
      -- The turn type to send out in mission commands
      TurnType : AFRL.CMASI.Enumerations.TurnTypeEnum := TurnShort;
      -- Minimun time between send in mission commands
      TimeBetweenMissionCommandsMin_ms : Common.Int64 := 1000;
      -- Payload Id to use for addressing the managed vehicle's gimbal
      GimbalPayloadId : Common.Int64 := -1;

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
