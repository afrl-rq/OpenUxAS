with AFRL.CMASI.AutomationResponse;                 use AFRL.CMASI.AutomationResponse;
with AFRL.CMASI.Enumerations;
with AFRL.CMASI.MissionCommand;                     use AFRL.CMASI.MissionCommand;
with AFRL.CMASI.ServiceStatus;                      use AFRL.CMASI.ServiceStatus;
with AFRL.CMASI.VehicleActionCommand;               use AFRL.CMASI.VehicleActionCommand;
with AFRL.CMASI.FlightDirectorAction;               use AFRL.CMASI.FlightDirectorAction;
with AFRL.Impact.ImpactAutomationResponse;          use AFRL.Impact.ImpactAutomationResponse;
with AVTAS.LMCP.Types;
with Common;
with UxAS.Messages.lmcptask.PlanningState;          use UxAS.Messages.lmcptask.PlanningState;
with UxAS.Messages.lmcptask.TaskAssignment;         use UxAS.Messages.lmcptask.TaskAssignment;
with UxAS.Messages.lmcptask.TaskAssignmentSummary;  use UxAS.Messages.lmcptask.TaskAssignmentSummary;
with UxAS.Messages.lmcptask.TaskAutomationResponse; use UxAS.Messages.lmcptask.TaskAutomationResponse;
with UxAS.Messages.lmcptask.TaskOptionCost;         use UxAS.Messages.lmcptask.TaskOptionCost;
with UxAS.Messages.Route.RouteResponse;             use UxAS.Messages.Route.RouteResponse;
with larcfm.DAIDALUS.Enumerations;
with definitions;

package body LMCP_Message_Conversions is

   -----------------------
   -- Local subprograms --
   -----------------------

   function As_AssignmentCostMatrix_Acc
     (Msg : LMCP_Messages.AssignmentCostMatrix'Class)
      return AssignmentCostMatrix_Acc;

   function As_AutomationResponse_Acc
     (Msg : LMCP_Messages.AutomationResponse'Class)
      return AutomationResponse_Acc;

   function As_ImpactAutomationResponse_Acc
     (Msg : LMCP_Messages.ImpactAutomationResponse'Class)
      return ImpactAutomationResponse_Acc;

   function As_KeyValuePair_Acc
     (Msg : LMCP_Messages.KeyValuePair)
      return KeyValuePair_Acc;

   function As_Location3D_Any
     (Msg : LMCP_Messages.Location3D)
      return Location3D_Any;

   function As_MissionCommand_Acc
     (Msg : LMCP_Messages.MissionCommand)
      return MissionCommand_Acc;

   --  function As_MissionCommand_Message
   --    (Msg : MissionCommand_Acc)
   --     return LMCP_Messages.MissionCommand;

   function As_RouteConstraints_Acc
     (Msg : LMCP_Messages.RouteConstraints)
      return RouteConstraints_Acc;

   function As_RoutePlanRequest_Acc
     (Msg : LMCP_Messages.RoutePlanRequest'Class)
      return RoutePlanRequest_Acc;

   function As_RoutePlanResponse_Acc
     (Msg : LMCP_Messages.RoutePlanResponse'Class)
      return RoutePlanResponse_Acc;

   function As_RouteRequest_Acc
     (Msg : LMCP_Messages.RouteRequest'Class)
      return RouteRequest_Acc;

   function As_RouteResponse_Acc
     (Msg : LMCP_Messages.RouteResponse'Class)
      return RouteResponse_Acc;

   function As_ServiceStatus_Acc
     (Msg : LMCP_Messages.ServiceStatus'Class)
      return ServiceStatus_Acc;

   function As_TaskAssignmentSummary_Acc
     (Msg : LMCP_Messages.TaskAssignmentSummary'Class)
      return TaskAssignmentSummary_Acc;

   function As_TaskAssignment_Acc
     (Msg : LMCP_Messages.TaskAssignment)
      return TaskAssignment_Acc;

   function As_TaskAutomationResponse_Acc
     (Msg : LMCP_Messages.TaskAutomationResponse'Class)
      return TaskAutomationResponse_Acc;

   function As_TaskOptionCost_Acc
     (Msg : LMCP_Messages.TaskOptionCost)
      return TaskOptionCost_Acc;

   function As_TaskOptionCost_Message
     (Arg : not null TaskOptionCost_Acc)
      return LMCP_Messages.TaskOptionCost;

   function As_UniqueAutomationRequest_Acc
     (Msg : LMCP_Messages.UniqueAutomationRequest'Class)
      return UniqueAutomationRequest_Acc;

   function As_VehicleActionCommand_Any
     (Msg : LMCP_Messages.VehicleActionCommand)
      return VehicleActionCommand_Any;

   function As_VehicleActionCommand_Acc
     (Msg : LMCP_Messages.VehicleActionCommand_w_FlightDirectorAction)
     return VehicleActionCommand_Acc;

   function As_VehicleActionCommand_Message
     (Msg : VehicleActionCommand_Any)
      return LMCP_Messages.VehicleActionCommand;

   function As_VehicleAction_Acc
     (Msg : LMCP_Messages.VehicleAction)
      return VehicleAction_Acc;

   function As_Waypoint_Acc
     (Msg : LMCP_Messages.Waypoint)
      return Waypoint_Acc;

   function As_FlightDirectorAction_Acc
     (Msg : LMCP_Messages.VehicleAction_Descendant_FlightDirectorAction)
      return FlightDirectorAction_Acc;

   --  function As_GroundHeadingInterval_Acc
   --    (Msg : LMCP_Messages.GroundHeadingInterval'Class)
   --     return GroundHeadingInterval_Acc;
   --
   --  function As_Vect_GroundHeadingInterval_Acc_Acc
   --    (Msg : Vect_GroundHeadingInterval_Acc_Acc'Class)
   --    return
   ---------------------------------
   -- As_AssignmentCostMatrix_Acc --
   ---------------------------------

   function As_AssignmentCostMatrix_Acc
     (Msg : LMCP_Messages.AssignmentCostMatrix'Class)
      return AssignmentCostMatrix_Acc
   is
      Result : constant AssignmentCostMatrix_Acc := new AssignmentCostMatrix;
      use AVTAS.LMCP.Types;
   begin
      Result.all.setCorrespondingAutomationRequestID (Int64 (Msg.CorrespondingAutomationRequestID));

      for TaskId of Msg.TaskList loop
         Result.getTaskList.Append (Int64 (TaskId));
      end loop;

      Result.all.setOperatingRegion (Int64 (Msg.OperatingRegion));

      for TOC of Msg.CostMatrix loop
         Result.getCostMatrix.Append (As_TaskOptionCost_Acc (TOC));

      end loop;

      return Result;
   end As_AssignmentCostMatrix_Acc;

   -------------------------------------
   -- As_AssignmentCostMatrix_Message --
   -------------------------------------

   function As_AssignmentCostMatrix_Message
     (Msg : not null AssignmentCostMatrix_Any)
      return LMCP_Messages.AssignmentCostMatrix
   is

      use all type Common.Int64_Seq;
      use all type LMCP_Messages.TOC_Seq;

      Result : LMCP_Messages.AssignmentCostMatrix;
   begin
      Result.CorrespondingAutomationRequestID := Common.Int64 (Msg.all.getCorrespondingAutomationRequestID);
      Result.OperatingRegion := Common.Int64 (Msg.all.getOperatingRegion);
      for TaskId of Msg.all.getTaskList.all loop
         Result.TaskList := Add (Result.TaskList, Common.Int64 (TaskId));
      end loop;

      for TaskOptionCost of Msg.all.getCostMatrix.all loop
         Result.CostMatrix := Add (Result.CostMatrix, As_TaskOptionCost_Message (TaskOptionCost));
      end loop;
      return Result;
   end As_AssignmentCostMatrix_Message;

   ----------------------------------
   -- As_AutomationRequest_Message --
   ----------------------------------

   function As_AutomationRequest_Message
     (Msg : not null AutomationRequest_Any)
      return LMCP_Messages.AutomationRequest
   is
      Result : LMCP_Messages.AutomationRequest;
      use Common;
   begin

      for EntityId of Msg.all.getEntityList.all loop
         Result.EntityList := Add (Result.EntityList, Int64 (EntityId));
      end loop;

      Result.OperatingRegion := Int64 (Msg.all.getOperatingRegion);

      for TaskId of Msg.all.getTaskList.all loop
         Result.TaskList := Add (Result.TaskList, Int64 (TaskId));
      end loop;

      Result.TaskRelationships := Msg.all.getTaskRelationships;

      Result.RedoAllTasks := Msg.all.getRedoAllTasks;

      return Result;
   end As_AutomationRequest_Message;

   -------------------------------
   -- As_AutomationResponse_Acc --
   -------------------------------

   function As_AutomationResponse_Acc
     (Msg : LMCP_Messages.AutomationResponse'Class)
      return AutomationResponse_Acc
   is
      Result : constant AutomationResponse_Acc := new AutomationResponse;
   begin
      for MissionCommand of Msg.MissionCommandList loop
         Result.getMissionCommandList.Append (As_MissionCommand_Acc (MissionCommand));
      end loop;

      for VehicleActionCommand of Msg.VehicleCommandList loop
         Result.getVehicleCommandList.Append (As_VehicleActionCommand_Any (VehicleActionCommand));
      end loop;

      for KVP of Msg.Info loop
         Result.getInfo.Append (As_KeyValuePair_Acc (KVP));
      end loop;

      return Result;
   end As_AutomationResponse_Acc;

   ----------------------------
   -- As_EntityState_Message --
   ----------------------------

   function As_EntityState_Message
     (Msg : not null EntityState_Any)
      return LMCP_Messages.EntityState
   is
      Result : LMCP_Messages.EntityState;
      use Common;
   begin
      Result.Id := Int64 (Msg.getID);
      Result.Location := As_Location3D_Message (Msg.getLocation);
      Result.Heading := Real32 (Msg.getHeading);
      Result.CurrentWaypoint := Int64 (Msg.getCurrentWaypoint);

      return Result;
   end As_EntityState_Message;

   ----------------------------------------
   -- As_ImpactAutomationRequest_Message --
   ----------------------------------------

   function As_ImpactAutomationRequest_Message
     (Msg : not null ImpactAutomationRequest_Any)
      return LMCP_Messages.ImpactAutomationRequest
   is
      Result : LMCP_Messages.ImpactAutomationRequest;
      use Common;
   begin
      Result.RequestID := Int64 (Msg.all.getRequestID);

      for EntityId of Msg.all.getTrialRequest.getEntityList.all loop
         Result.EntityList := Add (Result.EntityList, Int64 (EntityId));
      end loop;

      Result.OperatingRegion := Int64 (Msg.all.getTrialRequest.getOperatingRegion);

      for TaskId of Msg.all.getTrialRequest.getTaskList.all loop
         Result.TaskList := Add (Result.TaskList, Int64 (TaskId));
      end loop;

      Result.TaskRelationships := Msg.all.getTrialRequest.getTaskRelationships;
      Result.PlayID := Int64 (Msg.all.getPlayID);
      Result.SolutionID := Int64 (Msg.all.getSolutionID);
      Result.RedoAllTasks := Msg.all.getTrialRequest.getRedoAllTasks;

      return Result;
   end As_ImpactAutomationRequest_Message;

   -------------------------------------
   -- As_ImpactAutomationResponse_Acc --
   -------------------------------------

   function As_ImpactAutomationResponse_Acc
     (Msg : LMCP_Messages.ImpactAutomationResponse'Class)
      return ImpactAutomationResponse_Acc
   is
      Result : constant ImpactAutomationResponse_Acc := new ImpactAutomationResponse;
      use AVTAS.LMCP.Types;
   begin
      Result.setResponseID (Int64 (Msg.ResponseID));

      for MissionCommand of Msg.MissionCommandList loop
         Result.getTrialResponse.getMissionCommandList.Append (As_MissionCommand_Acc (MissionCommand));
      end loop;

      for VehicleActionCommand of Msg.VehicleCommandList loop
         Result.getTrialResponse.getVehicleCommandList.Append (As_VehicleActionCommand_Any (VehicleActionCommand));
      end loop;

      for KVP of Msg.Info loop
         Result.getTrialResponse.getInfo.Append (As_KeyValuePair_Acc (KVP));
      end loop;

      Result.setPlayID (Int64 (Msg.PlayId));
      Result.setSolutionID (Int64 (Msg.SolutionId));
      Result.setSandbox (Msg.Sandbox);

      return Result;
   end As_ImpactAutomationResponse_Acc;

   -------------------------
   -- As_KeyValuePair_Acc --
   -------------------------

   function As_KeyValuePair_Acc
     (Msg : LMCP_Messages.KeyValuePair)
      return KeyValuePair_Acc
   is
      Result : constant KeyValuePair_Acc := new KeyValuePair;
   begin
      Result.setKey (Msg.Key);
      Result.setValue (Msg.Value);
      return Result;
   end As_KeyValuePair_Acc;

   -----------------------------
   -- As_KeyValuePair_Message --
   -----------------------------

   function As_KeyValuePair_Message
     (Msg : not null KeyValuePair_Acc)
      return LMCP_Messages.KeyValuePair
   is
      Result : LMCP_Messages.KeyValuePair;
   begin
      Result.Key := Msg.getKey;
      Result.Value := Msg.getValue;
      return Result;
   end As_KeyValuePair_Message;

   -----------------------
   -- As_Location3D_Any --
   -----------------------

   function As_Location3D_Any
     (Msg : LMCP_Messages.Location3D)
      return Location3D_Any
   is
      Result : constant Location3D_Acc := new Location3D;
   begin
      Result.setLatitude (AVTAS.LMCP.Types.Real64 (Msg.Latitude));
      Result.setLongitude (AVTAS.LMCP.Types.Real64 (Msg.Longitude));
      Result.setAltitude (AVTAS.LMCP.Types.Real32 (Msg.Altitude));
      case Msg.AltitudeType is
         when LMCP_Messages.AGL => Result.setAltitudeType (AFRL.CMASI.Enumerations.AGL);
         when LMCP_Messages.MSL => Result.setAltitudeType (AFRL.CMASI.Enumerations.MSL);
      end case;

      return Location3D_Any (Result);
   end As_Location3D_Any;

   ---------------------------
   -- As_Location3D_Message --
   ---------------------------

   function As_Location3D_Message
     (Msg : not null Location3D_Any)
      return LMCP_Messages.Location3D
   is
      Result : LMCP_Messages.Location3D;
   begin
      Result.Latitude := Common.Real64 (Msg.getLatitude);
      Result.Longitude := Common.Real64 (Msg.getLongitude);
      Result.Altitude := Common.Real32 (Msg.getAltitude);
      --  For this enumeration type component we could use 'Val and 'Pos to
      --  convert the values, but that would not be robust in the face of
      --  independent changes to either one of the two enumeration type
      --  decls, especially the order. Therefore we do an explicit comparison.
      case Msg.getAltitudeType is
         when AFRL.CMASI.Enumerations.AGL => Result.AltitudeType := LMCP_Messages.AGL;
         when AFRL.CMASI.Enumerations.MSL => Result.AltitudeType := LMCP_Messages.MSL;
      end case;
      return Result;
   end As_Location3D_Message;

   ---------------------------
   -- As_MissionCommand_Acc --
   ---------------------------

   function As_MissionCommand_Acc
     (Msg : LMCP_Messages.MissionCommand)
      return MissionCommand_Acc
   is
      Result : constant MissionCommand_Acc := new MissionCommand;
      use AVTAS.LMCP.Types;
   begin
      Result.setCommandID (Int64 (Msg.CommandId));
      Result.setVehicleID (Int64 (Msg.VehicleId));

      for VehicleAction of Msg.VehicleActionList loop
         Result.getVehicleActionList.Append (VehicleAction_Any (As_VehicleAction_Acc (VehicleAction)));
      end loop;

      case Msg.Status is
         when LMCP_Messages.Pending => Result.setStatus (AFRL.CMASI.Enumerations.Pending);
         when LMCP_Messages.Approved => Result.setStatus (AFRL.CMASI.Enumerations.Approved);
         when LMCP_Messages.InProcess => Result.setStatus (AFRL.CMASI.Enumerations.InProcess);
         when LMCP_Messages.Executed => Result.setStatus (AFRL.CMASI.Enumerations.Executed);
         when LMCP_Messages.Cancelled => Result.setStatus (AFRL.CMASI.Enumerations.Cancelled);
      end case;

      for Waypoint of Msg.WaypointList loop
         Result.getWaypointList.Append (Waypoint_Any (As_Waypoint_Acc (Waypoint)));
      end loop;

      Result.setFirstWaypoint (Int64 (Msg.FirstWaypoint));

      return Result;
   end As_MissionCommand_Acc;

   -------------------------------
   -- As_MissionCommand_Message --
   -------------------------------

   function As_MissionCommand_Message
     (Msg : not null MissionCommand_Acc)
      return LMCP_Messages.MissionCommand
   is
      Result : LMCP_Messages.MissionCommand;
      use Common;
      use all type LMCP_Messages.VA_Seq;
      use all type LMCP_Messages.WP_Seq;
   begin
      Result.CommandId := Int64 (Msg.all.getCommandID);
      Result.VehicleId := Int64 (Msg.all.getVehicleID);

      for VehicleAction of Msg.all.getVehicleActionList.all loop
         Result.VehicleActionList :=
           Add (Result.VehicleActionList,
                As_VehicleAction_Message (VehicleAction));
      end loop;

      case Msg.all.getStatus is
         when AFRL.CMASI.Enumerations.Pending => Result.Status := LMCP_Messages.Pending;
         when AFRL.CMASI.Enumerations.Approved => Result.Status := LMCP_Messages.Approved;
         when AFRL.CMASI.Enumerations.InProcess => Result.Status := LMCP_Messages.InProcess;
         when AFRL.CMASI.Enumerations.Executed => Result.Status := LMCP_Messages.Executed;
         when AFRL.CMASI.Enumerations.Cancelled => Result.Status := LMCP_Messages.Cancelled;
      end case;

      for Waypoint of Msg.all.getWaypointList.all loop
         Result.WaypointList :=
           Add (Result.WaypointList,
                As_Waypoint_Message (Waypoint));
      end loop;

      Result.FirstWaypoint := Int64 (Msg.all.getFirstWaypoint);

      return Result;
   end As_MissionCommand_Message;

   -------------------
   -- As_Object_Any --
   -------------------

   function As_Object_Any
     (Msg : LMCP_Messages.Message_Root'Class)
      return AVTAS.LMCP.Object.Object_Any
   is
      Result : AVTAS.LMCP.Object.Object_Any;
   begin
      --  TODO: Consider using the stream 'Write routines (not the 'Output
      --  versions) to write the message objects to a byte array, then use
      --  Unpack to get the LMCP pointer type from that. We'd need a function
      --  mapping Message_Root tags to the LMCP enumeration identifying message
      --  types (which handles the necessary omission of writing the tags)

      if Msg in LMCP_Messages.RoutePlanRequest'Class then
         Result := AVTAS.LMCP.Object.Object_Any (As_RoutePlanRequest_Acc (LMCP_Messages.RoutePlanRequest'Class (Msg)));

      elsif Msg in LMCP_Messages.RoutePlanResponse'Class then
         Result := AVTAS.LMCP.Object.Object_Any (As_RoutePlanResponse_Acc (LMCP_Messages.RoutePlanResponse'Class (Msg)));

      elsif Msg in LMCP_Messages.RouteRequest'Class then
         Result := AVTAS.LMCP.Object.Object_Any (As_RouteRequest_Acc (LMCP_Messages.RouteRequest'Class (Msg)));

      elsif Msg in LMCP_Messages.RouteResponse'Class then
         Result := AVTAS.LMCP.Object.Object_Any (As_RouteResponse_Acc (LMCP_Messages.RouteResponse'Class (Msg)));

      elsif Msg in LMCP_Messages.AssignmentCostMatrix'Class then
         Result := AVTAS.LMCP.Object.Object_Any (As_AssignmentCostMatrix_Acc (LMCP_Messages.AssignmentCostMatrix'Class (Msg)));

      elsif Msg in LMCP_Messages.TaskAssignmentSummary'Class then
         Result := AVTAS.LMCP.Object.Object_Any (As_TaskAssignmentSummary_Acc (LMCP_Messages.TaskAssignmentSummary'Class (Msg)));

      elsif Msg in LMCP_Messages.UniqueAutomationRequest'Class then
         Result := AVTAS.LMCP.Object.Object_Any (As_UniqueAutomationRequest_Acc (LMCP_Messages.UniqueAutomationRequest'Class (Msg)));

      elsif Msg in LMCP_Messages.ServiceStatus'Class then
         Result := AVTAS.LMCP.Object.Object_Any (As_ServiceStatus_Acc (LMCP_Messages.ServiceStatus'Class (Msg)));

      elsif Msg in LMCP_Messages.ImpactAutomationResponse'Class then
         Result := AVTAS.LMCP.Object.Object_Any (As_ImpactAutomationResponse_Acc (LMCP_Messages.ImpactAutomationResponse'Class (Msg)));

      elsif Msg in LMCP_Messages.TaskAutomationResponse'Class then
         Result := AVTAS.LMCP.Object.Object_Any (As_TaskAutomationResponse_Acc (LMCP_Messages.TaskAutomationResponse'Class (Msg)));

      elsif Msg in LMCP_Messages.AutomationResponse'Class then
         Result := AVTAS.LMCP.Object.Object_Any (As_AutomationResponse_Acc (LMCP_Messages.AutomationResponse'Class (Msg)));

      elsif Msg in LMCP_Messages.MissionCommand'Class then
         Result := AVTAS.LMCP.Object.Object_Any (As_MissionCommand_Acc (LMCP_Messages.MissionCommand (Msg)));

      elsif Msg in LMCP_Messages.VehicleActionCommand_w_FlightDirectorAction'Class
      then
         Result := AVTAS.LMCP.Object.Object_Any
           (As_VehicleActionCommand_Acc
              (LMCP_Messages.VehicleActionCommand_w_FlightDirectorAction (Msg)));

      else
         raise Program_Error with "unexpected message kind in Route_Aggregator_Message_Conversions.As_Object_Any";
         --  UniqueAutomationRequest is in the class but not sent
      end if;

      return Result;
   end As_Object_Any;

   -----------------------------
   -- As_RouteConstraints_Acc --
   -----------------------------

   function As_RouteConstraints_Acc
     (Msg : LMCP_Messages.RouteConstraints)
      return RouteConstraints_Acc
   is
      Result : constant RouteConstraints_Acc := new RouteConstraints;
   begin
      Result.setRouteID (AVTAS.LMCP.Types.Int64 (Msg.RouteID));
      Result.setStartLocation (As_Location3D_Any (Msg.StartLocation));
      Result.setStartHeading (AVTAS.LMCP.Types.Real32 (Msg.StartHeading));
      Result.setUseStartHeading (Msg.UseStartHeading);
      Result.setEndLocation (As_Location3D_Any (Msg.EndLocation));
      Result.setEndHeading (AVTAS.LMCP.Types.Real32 (Msg.EndHeading));
      Result.setUseEndHeading (Msg.UseEndHeading);
      return Result;
   end As_RouteConstraints_Acc;

   ---------------------------------
   -- As_RouteConstraints_Message --
   ---------------------------------

   function As_RouteConstraints_Message
     (Msg : not null RouteConstraints_Any)
      return LMCP_Messages.RouteConstraints
   is
      Result : LMCP_Messages.RouteConstraints;
   begin
      Result.RouteID         := Common.Int64 (Msg.getRouteID);
      Result.StartLocation   := As_Location3D_Message (Msg.getStartLocation);
      Result.StartHeading    := Common.Real32 (Msg.getStartHeading);
      Result.UseStartHeading := Msg.getUseStartHeading;
      Result.EndLocation     := As_Location3D_Message (Msg.getEndLocation);
      Result.EndHeading      := Common.Real32 (Msg.getEndHeading);
      Result.UseEndHeading   := Msg.getUseEndHeading;
      return Result;
   end As_RouteConstraints_Message;

   ------------------------------
   -- As_RoutePlanRequest_Acc --
   ------------------------------

   function As_RoutePlanRequest_Acc
     (Msg : LMCP_Messages.RoutePlanRequest'Class)
      return RoutePlanRequest_Acc
   is
      Result : constant RoutePlanRequest_Acc := new RoutePlanRequest;
   begin
      Result.setRequestID (AVTAS.LMCP.Types.Int64 (Msg.RequestID));
      Result.setAssociatedTaskID (AVTAS.LMCP.Types.Int64 (Msg.AssociatedTaskID));
      Result.setVehicleID (AVTAS.LMCP.Types.Int64 (Msg.VehicleID));
      Result.setOperatingRegion (AVTAS.LMCP.Types.Int64 (Msg.OperatingRegion));
      Result.setIsCostOnlyRequest (Msg.IsCostOnlyRequest);
      for RC : LMCP_Messages.RouteConstraints of Msg.RouteRequests loop
         Result.getRouteRequests.Append (As_RouteConstraints_Acc (RC));
      end loop;
      return Result;
   end As_RoutePlanRequest_Acc;

   ---------------------------------
   -- As_RoutePlanRequest_Message --
   ---------------------------------

   function As_RoutePlanRequest_Message
     (Msg : not null RoutePlanRequest_Any)
      return LMCP_Messages.RoutePlanRequest
   is
      Result : LMCP_Messages.RoutePlanRequest;
   begin
      Result.RequestID         := Common.Int64 (Msg.getRequestID);
      Result.AssociatedTaskID  := Common.Int64 (Msg.getAssociatedTaskID);
      Result.VehicleID         := Common.Int64 (Msg.getVehicleID);
      Result.OperatingRegion   := Common.Int64 (Msg.getOperatingRegion);
      Result.IsCostOnlyRequest := Msg.getIsCostOnlyRequest;
      for RC of Msg.getRouteRequests.all loop
         Result.RouteRequests := LMCP_Messages.Add (Result.RouteRequests, As_RouteConstraints_Message (RouteConstraints_Any (RC)));
      end loop;
      return Result;
   end As_RoutePlanRequest_Message;

   ------------------------------
   -- As_RoutePlanResponse_Acc --
   ------------------------------

   function As_RoutePlanResponse_Acc
     (Msg : LMCP_Messages.RoutePlanResponse'Class)
      return RoutePlanResponse_Acc
   is
      Result         : constant RoutePlanResponse_Acc := new RoutePlanResponse;
      New_Route_Plan : UxAS.Messages.Route.RoutePlan.RoutePlan_Acc;
   begin
      Result.setResponseID (AVTAS.LMCP.Types.Int64 (Msg.ResponseID));
      Result.setAssociatedTaskID (AVTAS.LMCP.Types.Int64 (Msg.AssociatedTaskID));
      Result.setVehicleID (AVTAS.LMCP.Types.Int64 (Msg.VehicleID));
      Result.setOperatingRegion (AVTAS.LMCP.Types.Int64 (Msg.OperatingRegion));

      for Plan_Msg : LMCP_Messages.RoutePlan of Msg.RouteResponses loop
         New_Route_Plan := new UxAS.Messages.Route.RoutePlan.RoutePlan;

         New_Route_Plan.setRouteID (AVTAS.LMCP.Types.Int64 (Plan_Msg.RouteID));
         New_Route_Plan.setRouteCost (AVTAS.LMCP.Types.Int64 (Plan_Msg.RouteCost));

         --  waypoints...
         for WP : LMCP_Messages.Waypoint of Plan_Msg.Waypoints loop
            New_Route_Plan.getWaypoints.Append (Waypoint_Any (As_Waypoint_Acc (WP)));
         end loop;

         --  route errors...
         for KVP : LMCP_Messages.KeyValuePair of Plan_Msg.RouteError loop
            New_Route_Plan.getRouteError.Append (As_KeyValuePair_Acc (KVP));
         end loop;

         Result.getRouteResponses.Append (New_Route_Plan);
      end loop;

      return Result;
   end As_RoutePlanResponse_Acc;

   ----------------------------------
   -- As_RoutePlanResponse_Message --
   ----------------------------------

   function As_RoutePlanResponse_Message
     (Msg : not null RoutePlanResponse_Any)
      return LMCP_Messages.RoutePlanResponse
   is
      Result        : LMCP_Messages.RoutePlanResponse;
      New_RoutePlan : LMCP_Messages.RoutePlan;
      use LMCP_Messages;
      use Common;
   begin
      Result.ResponseID       := Int64 (Msg.getResponseID);
      Result.AssociatedTaskID := Int64 (Msg.getAssociatedTaskID);
      Result.VehicleID        := Int64 (Msg.getVehicleID);
      Result.OperatingRegion  := Int64 (Msg.getOperatingRegion);

      for Plan of Msg.getRouteResponses.all loop
         New_RoutePlan.RouteID   := Int64 (Plan.getRouteID);
         New_RoutePlan.RouteCost := Int64 (Plan.getRouteCost);

         for WP of Plan.getWaypoints.all loop
            New_RoutePlan.Waypoints := Add (New_RoutePlan.Waypoints, As_Waypoint_Message (WP));
         end loop;

         for Error of Plan.getRouteError.all loop
            New_RoutePlan.RouteError := Add (New_RoutePlan.RouteError, As_KeyValuePair_Message (Error));
         end loop;

         Result.RouteResponses := Add (Result.RouteResponses, New_RoutePlan);
      end loop;

      return Result;
   end As_RoutePlanResponse_Message;

   --------------------------
   -- As_RoutePlan_Message --
   --------------------------

   function As_RoutePlan_Message
     (Msg : not null RoutePlan_Any)
      return LMCP_Messages.RoutePlan
   is
      Result : LMCP_Messages.RoutePlan;
      use LMCP_Messages;
   begin
      Result.RouteID := Common.Int64 (Msg.getRouteID);
      for WP of Msg.getWaypoints.all loop
         Result.Waypoints := Add (Result.Waypoints, As_Waypoint_Message (WP));
      end loop;
      Result.RouteCost := Common.Int64 (Msg.getRouteCost);
      for Error of Msg.getRouteError.all loop
         Result.RouteError := Add (Result.RouteError, As_KeyValuePair_Message (Error));
      end loop;
      return Result;
   end As_RoutePlan_Message;

   -------------------------
   -- As_RouteRequest_Acc --
   -------------------------

   function As_RouteRequest_Acc
     (Msg : LMCP_Messages.RouteRequest'Class)
      return RouteRequest_Acc
   is
      Result : constant RouteRequest_Acc := new RouteRequest;
   begin
      Result.setRequestID (AVTAS.LMCP.Types.Int64 (Msg.RequestID));
      Result.setAssociatedTaskID (AVTAS.LMCP.Types.Int64 (Msg.AssociatedTaskID));
      for VID of Msg.VehicleID loop
         Result.getVehicleID.Append (AVTAS.LMCP.Types.Int64 (VID));
      end loop;
      Result.setOperatingRegion (AVTAS.LMCP.Types.Int64 (Msg.OperatingRegion));
      Result.setIsCostOnlyRequest (Msg.IsCostOnlyRequest);
      for RC of Msg.RouteRequests loop
         Result.getRouteRequests.Append (As_RouteConstraints_Acc (RC));
      end loop;

      return Result;
   end As_RouteRequest_Acc;

   -----------------------------
   -- As_RouteRequest_Message --
   -----------------------------

   function As_RouteRequest_Message
     (Msg : not null RouteRequest_Any)
      return LMCP_Messages.RouteRequest
   is
      Result : LMCP_Messages.RouteRequest;
   begin
      Result.RequestID        := Common.Int64 (Msg.getRequestID);
      Result.AssociatedTaskID := Common.Int64 (Msg.getAssociatedTaskID);
      for VID of Msg.getVehicleID.all loop
         Result.VehicleID := Common.Add (Result.VehicleID, Common.Int64 (VID));
      end loop;
      Result.OperatingRegion   := Common.Int64 (Msg.getOperatingRegion);
      Result.IsCostOnlyRequest := Msg.getIsCostOnlyRequest;
      for RC of Msg.getRouteRequests.all loop
         Result.RouteRequests := LMCP_Messages.Add (Result.RouteRequests, As_RouteConstraints_Message (RouteConstraints_Any (RC)));
      end loop;

      return Result;
   end As_RouteRequest_Message;

   --------------------------
   -- As_RouteResponse_Acc --
   --------------------------

   function As_RouteResponse_Acc
     (Msg : LMCP_Messages.RouteResponse'Class)
      return RouteResponse_Acc
   is
      Result : constant RouteResponse_Acc := new RouteResponse;
   begin
      Result.setResponseID (AVTAS.LMCP.Types.Int64 (Msg.ResponseID));
      for RP : LMCP_Messages.RoutePlanResponse of Msg.Routes loop
         Result.getRoutes.Append (As_RoutePlanResponse_Acc (RP));
      end loop;

      return Result;
   end As_RouteResponse_Acc;

   --------------------------
   -- As_ServiceStatus_Acc --
   --------------------------

   function As_ServiceStatus_Acc
     (Msg : LMCP_Messages.ServiceStatus'Class)
      return ServiceStatus_Acc
   is
      Result : constant ServiceStatus_Acc := new ServiceStatus;
      use AVTAS.LMCP.Types;
   begin
      Result.setPercentComplete (Real32 (Msg.PercentComplete));

      for KVP of Msg.Info loop
         Result.getInfo.Append (As_KeyValuePair_Acc (KVP));
      end loop;

      case Msg.StatusType is
         when LMCP_Messages.Information => Result.setStatusType (AFRL.CMASI.Enumerations.Information);
         when LMCP_Messages.Warning => Result.setStatusType (AFRL.CMASI.Enumerations.Warning);
         when LMCP_Messages.Error => Result.setStatusType (AFRL.CMASI.Enumerations.Error);
      end case;

      return Result;
   end As_ServiceStatus_Acc;

   ----------------------------------
   -- As_TaskAssignmentSummary_Acc --
   ----------------------------------

   function As_TaskAssignmentSummary_Acc
     (Msg : LMCP_Messages.TaskAssignmentSummary'Class)
      return TaskAssignmentSummary_Acc
   is
      Result : constant TaskAssignmentSummary_Acc := new TaskAssignmentSummary;
      use AVTAS.LMCP.Types;
   begin
      Result.setCorrespondingAutomationRequestID (Int64 (Msg.CorrespondingAutomationRequestID));
      Result.setOperatingRegion (Int64 (Msg.OperatingRegion));

      for TaskAssignment of Msg.TaskList loop
         Result.getTaskList.Append (As_TaskAssignment_Acc (TaskAssignment));
      end loop;
      return Result;
   end As_TaskAssignmentSummary_Acc;

   ---------------------------
   -- As_TaskAssignment_Acc --
   ---------------------------

   function As_TaskAssignment_Acc
     (Msg : LMCP_Messages.TaskAssignment)
      return TaskAssignment_Acc
   is
      Result : constant TaskAssignment_Acc := new TaskAssignment;
      use AVTAS.LMCP.Types;
   begin
      Result.setTaskID (Int64 (Msg.TaskID));
      Result.setOptionID (Int64 (Msg.OptionID));
      Result.setAssignedVehicle (Int64 (Msg.AssignedVehicle));
      Result.setTimeThreshold (Int64 (Msg.TimeThreshold));
      Result.setTimeTaskCompleted (Int64 (Msg.TimeTaskCompleted));

      return Result;
   end As_TaskAssignment_Acc;

   --------------------------------------
   -- As_TaskAutomationRequest_Message --
   --------------------------------------

   function As_TaskAutomationRequest_Message
     (Msg : not null TaskAutomationRequest_Any)
      return LMCP_Messages.TaskAutomationRequest
   is
      Result : LMCP_Messages.TaskAutomationRequest;
      use Common;
      use all type LMCP_Messages.PlanningState_Seq;
   begin
      Result.RequestID := Int64 (Msg.all.getRequestID);

      for EntityId of Msg.all.getOriginalRequest.getEntityList.all loop
         Result.EntityList := Add (Result.EntityList, Int64 (EntityId));
      end loop;

      Result.OperatingRegion := Int64 (Msg.all.getOriginalRequest.getOperatingRegion);

      for MsgPlanningState of Msg.all.getPlanningStates.all loop
         declare
            PlanningState : LMCP_Messages.PlanningState;
         begin
            PlanningState.EntityID := Int64 (MsgPlanningState.all.getEntityID);
            PlanningState.PlanningPosition := As_Location3D_Message (MsgPlanningState.all.getPlanningPosition);
            PlanningState.PlanningHeading := Real32 (MsgPlanningState.all.getPlanningHeading);

            Result.PlanningStates := Add (Result.PlanningStates, PlanningState);
         end;
      end loop;

      for TaskId of Msg.all.getOriginalRequest.getTaskList.all loop
         Result.TaskList := Add (Result.TaskList, Int64 (TaskId));
      end loop;

      Result.TaskRelationships := Msg.all.getOriginalRequest.getTaskRelationships;

      Result.RedoAllTasks := Msg.all.getOriginalRequest.getRedoAllTasks;

      return Result;
   end As_TaskAutomationRequest_Message;

   -----------------------------------
   -- As_TaskAutomationResponse_Acc --
   -----------------------------------

   function As_TaskAutomationResponse_Acc
     (Msg : LMCP_Messages.TaskAutomationResponse'Class)
      return TaskAutomationResponse_Acc
   is
      Result : constant TaskAutomationResponse_Acc := new TaskAutomationResponse;
      use AVTAS.LMCP.Types;
   begin
      Result.setResponseID (Int64 (Msg.ResponseID));

      for MissionCommand of Msg.MissionCommandList loop
         Result.getOriginalResponse.getMissionCommandList.Append (As_MissionCommand_Acc (MissionCommand));
      end loop;

      for VehicleActionCommand of Msg.VehicleCommandList loop
         Result.getOriginalResponse.getVehicleCommandList.Append (As_VehicleActionCommand_Any (VehicleActionCommand));
      end loop;

      for KVP of Msg.Info loop
         Result.getOriginalResponse.getInfo.Append (As_KeyValuePair_Acc (KVP));
      end loop;

      for Msg_FState of Msg.FinalStates loop
         declare
            FinalState : constant PlanningState_Acc := new PlanningState;
         begin
            FinalState.setEntityID (Int64 (Msg_FState.EntityID));
            FinalState.setPlanningPosition (As_Location3D_Any (Msg_FState.PlanningPosition));
            FinalState.setPlanningHeading (Real32 (Msg_FState.PlanningHeading));
            Result.getFinalStates.Append (FinalState);
         end;
      end loop;

      return Result;
   end As_TaskAutomationResponse_Acc;

   ---------------------------
   -- As_TaskOptionCost_Acc --
   ---------------------------

   function As_TaskOptionCost_Acc
     (Msg : LMCP_Messages.TaskOptionCost)
      return TaskOptionCost_Acc
   is
      Result : constant TaskOptionCost_Acc := new TaskOptionCost;
      use AVTAS.LMCP.Types;
   begin
      Result.all.setVehicleID (Int64 (Msg.VehicleID));
      Result.all.setIntialTaskID (Int64 (Msg.InitialTaskID));
      Result.all.setIntialTaskOption (Int64 (Msg.InitialTaskOption));
      Result.all.setDestinationTaskID (Int64 (Msg.DestinationTaskID));
      Result.all.setDestinationTaskOption (Int64 (Msg.DestinationTaskOption));
      Result.all.setTimeToGo (Int64 (Msg.TimeToGo));
      return Result;
   end As_TaskOptionCost_Acc;

   -------------------------------
   -- As_TaskOptionCost_Message --
   -------------------------------

   function As_TaskOptionCost_Message
     (Arg : not null TaskOptionCost_Acc)
      return LMCP_Messages.TaskOptionCost
   is
      Result : LMCP_Messages.TaskOptionCost;
   begin
      Result.VehicleID := Common.Int64 (Arg.getVehicleID);
      Result.InitialTaskID := Common.Int64 (Arg.getIntialTaskID);
      Result.InitialTaskOption := Common.Int64 (Arg.getIntialTaskOption);
      Result.DestinationTaskID := Common.Int64 (Arg.getDestinationTaskID);
      Result.DestinationTaskOption := Common.Int64 (Arg.getDestinationTaskOption);
      Result.TimeToGo := Common.Int64 (Arg.getTimeToGo);
      return Result;
   end As_TaskOptionCost_Message;

   -------------------------------
   -- As_TaskPlanOption_Message --
   -------------------------------

   function As_TaskPlanOption_Message
     (Msg : not null TaskPlanOptions_Any)
      return LMCP_Messages.TaskPlanOptions
   is
      Result : LMCP_Messages.TaskPlanOptions;
      use Common;
      use all type Int64_Seq;
      use all type LMCP_Messages.TaskOption_Seq;
   begin
      Result.CorrespondingAutomationRequestID :=
        Int64 (Msg.getCorrespondingAutomationRequestID);
      Result.TaskID :=
        Int64 (Msg.getTaskID);
      Result.Composition := Msg.getComposition;

      for MsgOption of Msg.getOptions.all loop
         declare
            Option : LMCP_Messages.TaskOption;
         begin
            Option.TaskID := Int64 (MsgOption.all.getTaskID);
            Option.OptionID := Int64 (MsgOption.all.getOptionID);
            Option.Cost := Int64 (MsgOption.all.getCost);
            Option.StartLocation :=
              As_Location3D_Message (MsgOption.all.getStartLocation);
            Option.StartHeading := Real32 (MsgOption.all.getStartHeading);
            Option.EndLocation :=
              As_Location3D_Message (MsgOption.all.getEndLocation);
            Option.EndHeading := Real32 (MsgOption.all.getEndHeading);

            for Entity of MsgOption.all.getEligibleEntities.all loop
               Option.EligibleEntities := Add (Option.EligibleEntities, Int64 (Entity));
            end loop;

            Result.Options := Add (Result.Options, Option);
         end;
      end loop;
      return Result;
   end As_TaskPlanOption_Message;

   ------------------------------------
   -- As_UniqueAutomationRequest_Acc --
   ------------------------------------

   function As_UniqueAutomationRequest_Acc
     (Msg : LMCP_Messages.UniqueAutomationRequest'Class)
      return UniqueAutomationRequest_Acc
   is
      Result : constant UniqueAutomationRequest_Acc := new UniqueAutomationRequest;
      use AVTAS.LMCP.Types;
   begin
      for Msg_PState of Msg.PlanningStates loop
         declare
            PState : constant PlanningState_Acc := new PlanningState;
         begin
            PState.all.setEntityID (Int64 (Msg_PState.EntityID));
            PState.all.setPlanningPosition (As_Location3D_Any (Msg_PState.PlanningPosition));
            PState.all.setPlanningHeading (Real32 (Msg_PState.PlanningHeading));
            Result.all.getPlanningStates.Append (PState);
         end;
      end loop;

      for EntityId of Msg.EntityList loop
         Result.getOriginalRequest.getEntityList.Append (Int64 (EntityId));
      end loop;

      for TaskId of Msg.TaskList loop
         Result.getOriginalRequest.getTaskList.Append (Int64 (TaskId));
      end loop;

      Result.all.setRequestID (Int64 (Msg.RequestID));
      Result.all.getOriginalRequest.setTaskRelationships (Msg.TaskRelationships);
      Result.all.getOriginalRequest.setOperatingRegion (Int64 (Msg.OperatingRegion));
      Result.all.getOriginalRequest.setRedoAllTasks (Msg.RedoAllTasks);
      Result.all.setSandBoxRequest (Msg.SandboxRequest);

      return Result;
   end As_UniqueAutomationRequest_Acc;

   ----------------------------------------
   -- As_UniqueAutomationRequest_Message --
   ----------------------------------------

   function As_UniqueAutomationRequest_Message
     (Msg : not null UniqueAutomationRequest_Any)
      return LMCP_Messages.UniqueAutomationRequest
   is
      Result : LMCP_Messages.UniqueAutomationRequest;
      use Common;
      use all type LMCP_Messages.PlanningState_Seq;
   begin
      Result.RequestID := Int64 (Msg.all.getRequestID);

      for EntityId of Msg.all.getOriginalRequest.getEntityList.all loop
         Result.EntityList := Add (Result.EntityList, Int64 (EntityId));
      end loop;

      Result.OperatingRegion := Int64 (Msg.all.getOriginalRequest.getOperatingRegion);

      for MsgPlanningState of Msg.all.getPlanningStates.all loop
         declare
            PlanningState : LMCP_Messages.PlanningState;
         begin
            PlanningState.EntityID := Int64 (MsgPlanningState.all.getEntityID);
            PlanningState.PlanningPosition := As_Location3D_Message (MsgPlanningState.all.getPlanningPosition);
            PlanningState.PlanningHeading := Real32 (MsgPlanningState.all.getPlanningHeading);

            Result.PlanningStates := Add (Result.PlanningStates, PlanningState);
         end;
      end loop;

      for TaskId of Msg.all.getOriginalRequest.getTaskList.all loop
         Result.TaskList := Add (Result.TaskList, Int64 (TaskId));
      end loop;

      Result.TaskRelationships := Msg.all.getOriginalRequest.getTaskRelationships;

      Result.RedoAllTasks := Msg.all.getOriginalRequest.getRedoAllTasks;

      return Result;

   end As_UniqueAutomationRequest_Message;

   -----------------------------------------
   -- As_UniqueAutomationResponse_Message --
   -----------------------------------------

   function As_UniqueAutomationResponse_Message
     (Msg : not null UniqueAutomationResponse_Any)
      return LMCP_Messages.UniqueAutomationResponse
   is
      Result : LMCP_Messages.UniqueAutomationResponse;
      use Common;
      use all type LMCP_Messages.PlanningState_Seq;
      use all type LMCP_Messages.MissionCommand_Seq;
      use all type LMCP_Messages.VehicleActionCommand_Seq;
      use all type LMCP_Messages.KVP_Seq;
   begin
      for MissionCommand of Msg.all.getOriginalResponse.getMissionCommandList.all loop
         Result.MissionCommandList :=
           Add (Result.MissionCommandList,
                As_MissionCommand_Message (MissionCommand));
      end loop;

      for VehicleActionCommand of Msg.all.getOriginalResponse.getVehicleCommandList.all loop
         Result.VehicleCommandList :=
           Add (Result.VehicleCommandList,
                As_VehicleActionCommand_Message (VehicleActionCommand));
      end loop;

      for KVP of Msg.all.getOriginalResponse.getInfo.all loop
         Result.Info := Add (Result.Info, As_KeyValuePair_Message (KVP));
      end loop;

      Result.ResponseID := Int64 (Msg.all.getResponseID);

      for MsgFinalState of Msg.all.getFinalStates.all loop
         declare
            FinalState : LMCP_Messages.PlanningState;
         begin
            FinalState.EntityID := Int64 (MsgFinalState.all.getEntityID);
            FinalState.PlanningPosition := As_Location3D_Message (MsgFinalState.all.getPlanningPosition);
            FinalState.PlanningHeading := Real32 (MsgFinalState.all.getPlanningHeading);

            Result.FinalStates := Add (Result.FinalStates, FinalState);
         end;
      end loop;

      return Result;
   end As_UniqueAutomationResponse_Message;

   ---------------------------------
   -- As_VehicleActionCommand_Any --
   ---------------------------------

   function As_VehicleActionCommand_Any
     (Msg : LMCP_Messages.VehicleActionCommand)
      return VehicleActionCommand_Any
   is
      Result : constant VehicleActionCommand_Any := new VehicleActionCommand;
      use AVTAS.LMCP.Types;
   begin
      Result.setCommandID (Int64 (Msg.CommandId));
      Result.setVehicleID (Int64 (Msg.VehicleId));

      for VehicleAction of Msg.VehicleActionList loop
         Result.getVehicleActionList.Append (VehicleAction_Any (As_VehicleAction_Acc (VehicleAction)));
      end loop;

      case Msg.Status is
         when LMCP_Messages.Pending => Result.setStatus (AFRL.CMASI.Enumerations.Pending);
         when LMCP_Messages.Approved => Result.setStatus (AFRL.CMASI.Enumerations.Approved);
         when LMCP_Messages.InProcess => Result.setStatus (AFRL.CMASI.Enumerations.InProcess);
         when LMCP_Messages.Executed => Result.setStatus (AFRL.CMASI.Enumerations.Executed);
         when LMCP_Messages.Cancelled => Result.setStatus (AFRL.CMASI.Enumerations.Cancelled);
      end case;

      return Result;
   end As_VehicleActionCommand_Any;

   ---------------------------------
   -- As_VehicleActionCommand_Acc --
   ---------------------------------

   function As_VehicleActionCommand_Acc
     (Msg : LMCP_Messages.VehicleActionCommand_w_FlightDirectorAction)
      return VehicleActionCommand_Acc
   is
      Result : constant VehicleActionCommand_Acc := new VehicleActionCommand;
      use AVTAS.LMCP.Types;
   begin
      Result.setCommandID (Int64 (Msg.CommandId));
      Result.setVehicleID (Int64 (Msg.VehicleId));

      for VehicleAction of Msg.VehicleActionList loop
         Result.getVehicleActionList.Append (VehicleAction_Any
                                             (As_FlightDirectorAction_Acc
                                                (VehicleAction)));
      end loop;

      case Msg.Status is
         when LMCP_Messages.Pending => Result.setStatus (AFRL.CMASI.Enumerations
                                                         .Pending);
         when LMCP_Messages.Approved => Result.setStatus
              (AFRL.CMASI.Enumerations.Approved);
         when LMCP_Messages.InProcess => Result.setStatus
              (AFRL.CMASI.Enumerations.InProcess);
         when LMCP_Messages.Executed => Result.setStatus
              (AFRL.CMASI.Enumerations.Executed);
         when LMCP_Messages.Cancelled => Result.setStatus
              (AFRL.CMASI.Enumerations.Cancelled);
      end case;

      return Result;
   end As_VehicleActionCommand_Acc;

   -------------------------------------
   -- As_VehicleActionCommand_Message --
   -------------------------------------

   function As_VehicleActionCommand_Message
     (Msg : VehicleActionCommand_Any)
      return LMCP_Messages.VehicleActionCommand
   is
      Result : LMCP_Messages.VehicleActionCommand;
      use Common;
      use all type LMCP_Messages.VA_Seq;
   begin
      Result.CommandId := Int64 (Msg.all.getCommandID);
      Result.VehicleId := Int64 (Msg.all.getVehicleID);

      for VehicleAction of Msg.all.getVehicleActionList.all loop
         Result.VehicleActionList :=
           Add (Result.VehicleActionList,
                As_VehicleAction_Message (VehicleAction));
      end loop;

      case Msg.all.getStatus is
         when AFRL.CMASI.Enumerations.Pending => Result.Status := LMCP_Messages.Pending;
         when AFRL.CMASI.Enumerations.Approved => Result.Status := LMCP_Messages.Approved;
         when AFRL.CMASI.Enumerations.InProcess => Result.Status := LMCP_Messages.InProcess;
         when AFRL.CMASI.Enumerations.Executed => Result.Status := LMCP_Messages.Executed;
         when AFRL.CMASI.Enumerations.Cancelled => Result.Status := LMCP_Messages.Cancelled;
      end case;

      return Result;
   end As_VehicleActionCommand_Message;

   --------------------------
   -- As_VehicleAction_Acc --
   --------------------------

   function As_VehicleAction_Acc
     (Msg : LMCP_Messages.VehicleAction)
      return VehicleAction_Acc
   is
      Result : constant VehicleAction_Acc := new VehicleAction;
   begin
      for Id : Common.Int64 of Msg.AssociatedTaskList loop
         Result.getAssociatedTaskList.Append (AVTAS.LMCP.Types.Int64 (Id));
      end loop;
      return Result;
   end As_VehicleAction_Acc;

   ---------------------------------
   -- As_FlightDirectorAction_Acc --
   ---------------------------------

   function As_FlightDirectorAction_Acc
     (Msg : LMCP_Messages.VehicleAction_Descendant_FlightDirectorAction)
      return FlightDirectorAction_Acc
   is
      use LMCP_Messages;
      Result : constant FlightDirectorAction_Acc := new FlightDirectorAction;
   begin

      for Id : Common.Int64 of Msg.AssociatedTaskList loop
         Result.getAssociatedTaskList.Append (AVTAS.LMCP.Types.Int64 (Id));
      end loop;
      Result.setSpeed (AVTAS.LMCP.Types.Real32 (Msg.Speed_mps));
      Result.setHeading (AVTAS.LMCP.Types.Real32 (Msg.Heading_deg));
      Result.setAltitude (AVTAS.LMCP.Types.Real32 (Msg.Altitude_m));
      Result.setClimbRate (AVTAS.LMCP.Types.Real32 (Msg.ClimbRate_mps));
      case Msg.AltitudeType is
         when AGL => Result.setAltitudeType (AFRL.CMASI.Enumerations.AGL);
         when MSL => Result.setAltitudeType (AFRL.CMASI.Enumerations.MSL);
      end case;
      case Msg.SpeedType is
         when Airspeed => Result.setSpeedType
              (AFRL.CMASI.Enumerations.Airspeed);
         when Groundspeed => Result.setSpeedType
              (AFRL.CMASI.Enumerations.Groundspeed);
      end case;
      return Result;
   end As_FlightDirectorAction_Acc;

   ------------------------------
   -- As_VehicleAction_Message --
   ------------------------------

   function As_VehicleAction_Message
     (Msg : not null VehicleAction_Any)
      return LMCP_Messages.VehicleAction
   is
      Result : LMCP_Messages.VehicleAction;
      use Common;
   begin
      for VA of Msg.getAssociatedTaskList.all loop
         Result.AssociatedTaskList := Add (Result.AssociatedTaskList, Common.Int64 (VA));
      end loop;
      return Result;
   end As_VehicleAction_Message;

   ---------------------
   -- As_Waypoint_Acc --
   ---------------------

   function As_Waypoint_Acc
     (Msg : LMCP_Messages.Waypoint)
      return Waypoint_Acc
   is
      Result : constant Waypoint_Acc := new AFRL.CMASI.Waypoint.Waypoint;
   begin
      --  the Location3D components
      Result.setLatitude (AVTAS.LMCP.Types.Real64 (Msg.Latitude));
      Result.setLongitude (AVTAS.LMCP.Types.Real64 (Msg.Longitude));
      Result.setAltitude (AVTAS.LMCP.Types.Real32 (Msg.Altitude));
      case Msg.AltitudeType is
         when LMCP_Messages.AGL => Result.setAltitudeType (AFRL.CMASI.Enumerations.AGL);
         when LMCP_Messages.MSL => Result.setAltitudeType (AFRL.CMASI.Enumerations.MSL);
      end case;

      --  the waypoint extensions

      Result.setNumber (AVTAS.LMCP.Types.Int64 (Msg.Number));
      Result.setNextWaypoint (AVTAS.LMCP.Types.Int64 (Msg.NextWaypoint));
      Result.setSpeed (AVTAS.LMCP.Types.Real32 (Msg.Speed));

      case Msg.SpeedType is
         when LMCP_Messages.Airspeed    => Result.setSpeedType (AFRL.CMASI.Enumerations.Airspeed);
         when LMCP_Messages.Groundspeed => Result.setSpeedType (AFRL.CMASI.Enumerations.Groundspeed);
      end case;

      Result.setClimbRate (AVTAS.LMCP.Types.Real32 (Msg.ClimbRate));

      case Msg.TurnType is
         when LMCP_Messages.TurnShort => Result.setTurnType (AFRL.CMASI.Enumerations.TurnShort);
         when LMCP_Messages.FlyOver   => Result.setTurnType (AFRL.CMASI.Enumerations.FlyOver);
      end case;

      for VA of Msg.VehicleActionList loop
         Result.getVehicleActionList.Append (VehicleAction_Any (As_VehicleAction_Acc (VA)));
      end loop;

      Result.setContingencyWaypointA (AVTAS.LMCP.Types.Int64 (Msg.ContingencyWaypointA));
      Result.setContingencyWaypointB (AVTAS.LMCP.Types.Int64 (Msg.ContingencyWaypointB));

      for Id of Msg.AssociatedTasks loop
         Result.getAssociatedTasks.Append (AVTAS.LMCP.Types.Int64 (Id));
      end loop;

      return Result;
   end As_Waypoint_Acc;

   -------------------------
   -- As_Waypoint_Message --
   -------------------------

   function As_Waypoint_Message
     (Msg : not null Waypoint_Any)
      return LMCP_Messages.Waypoint
   is
      Result : LMCP_Messages.Waypoint;
   begin
      --  the Location3D components
      LMCP_Messages.Location3D (Result) := As_Location3D_Message (Location3D_Any (Msg));

      --  the Waypoint extension components
      Result.Number := Common.Int64 (Msg.getNumber);
      Result.NextWaypoint := Common.Int64 (Msg.getNextWaypoint);
      Result.Speed := Common.Real32 (Msg.getSpeed);

      case Msg.getSpeedType is
         when AFRL.CMASI.Enumerations.Airspeed    => Result.SpeedType := LMCP_Messages.Airspeed;
         when AFRL.CMASI.Enumerations.Groundspeed => Result.SpeedType := LMCP_Messages.Groundspeed;
      end case;

      Result.ClimbRate := Common.Real32 (Msg.getClimbRate);

      case Msg.getTurnType is
         when AFRL.CMASI.Enumerations.TurnShort => Result.TurnType := LMCP_Messages.TurnShort;
         when AFRL.CMASI.Enumerations.FlyOver   => Result.TurnType := LMCP_Messages.FlyOver;
      end case;

      for VA of Msg.getVehicleActionList.all loop
         Result.VehicleActionList := LMCP_Messages.Add (Result.VehicleActionList, As_VehicleAction_Message (VA));
      end loop;

      Result.ContingencyWaypointA := Common.Int64 (Msg.getContingencyWaypointA);
      Result.ContingencyWaypointB := Common.Int64 (Msg.getContingencyWaypointB);

      for Id of Msg.getAssociatedTasks.all loop
         Result.AssociatedTasks := Common.Add (Result.AssociatedTasks, Common.Int64 (Id));
      end loop;

      return Result;
   end As_Waypoint_Message;

   --------------------------------------
   -- As_DAIDALUSConfiguration_Message --
   --------------------------------------

   function As_DAIDALUSConfiguration_Message
     (Msg : not null DAIDALUSConfiguration_Any)
      return LMCP_Messages.DAIDALUSConfiguration
   is
      Result : LMCP_Messages.DAIDALUSConfiguration;
   begin
      Result.EntityID := definitions.VehicleID_type (Msg.all.getEntityId);
      Result.LookAheadTime := definitions.ttlowc_sec (Msg.all.getLookAheadTime);
      Result.LeftTrack := definitions.Heading_Type_deg (Msg.all.getLeftTrack);
      Result.RightTrack := definitions.Heading_Type_deg (Msg.all.getRightTrack);
      Result.MaxGroundSpeed := definitions.GroundSpeed_Type_mps (Msg.all.getMaxGroundSpeed);
      Result.MinGroundSpeed := definitions.GroundSpeed_Type_mps (Msg.all.getMinGroundSpeed);
      Result.MaxVerticalSpeed := definitions.VerticalSpeed_Type_mps (Msg.all.getMaxVerticalSpeed);
      Result.MinVerticalSpeed := definitions.VerticalSpeed_Type_mps (Msg.all.getMinVerticalSpeed);
      Result.MaxAltitude := definitions.Altitude_Type_m (Msg.all.getMaxAltitude);
      Result.MinAltitude := definitions.Altitude_Type_m (Msg.all.getMinAltitude);
      Result.TrackStep := definitions.Heading_Buffer_Type_deg (Msg.all.getTrackStep);
      Result.GroundSpeedStep := definitions.GroundSpeed_Buffer_Type_mps (Msg.all.getGroundSpeedStep);
      Result.VerticalSpeedStep := definitions.VerticalSpeed_Buffer_Type_mps (Msg.all.getVerticalSpeedStep);
      Result.AltitudeStep := definitions.Altitude_Buffer_Type_m (Msg.all.getAltitudeStep);
      Result.HorizontalAcceleration := definitions.SafeReal64
        (Msg.all.getHorizontalAcceleration);
      Result.VerticalAcceleration := definitions.SafeReal64
        (Msg.all.getVerticalAcceleration);
      Result.TurnRate := definitions.SafeReal64 (Msg.all.getTurnRate);
      Result.BankAngle := definitions.Heading_Type_deg (Msg.all.getBankAngle);
      Result.VerticalRate := definitions.VerticalSpeed_Type_mps (Msg.all.getVerticalRate);
      Result.RecoveryStabilityTime := definitions.SafeReal64
        (Msg.all.getRecoveryStabilityTime);
      Result.isRecoveryTrackBands := Msg.all.getisRecoveryTrackBands;
      Result.isRecoveryGroundSpeedBands := Msg.all.getisRecoveryGroundSpeedBands;
      Result.isRecoveryVerticalSpeedBands :=
        Msg.all.getisRecoveryVerticalSpeedBands;
      Result.isRecoveryAltitudeBands := Msg.all.getisRecoveryAltitudeBands;
      Result.isCollisionAvoidanceBands := Msg.all.getisCollisionAvoidanceBands;
      Result.CollisionAvoidanceBandsFactor :=
        Msg.all.getCollisionAvoidanceBandsFactor;
      Result.HorizontalNMAC := definitions.SafeReal64 (Msg.all.getHorizontalNMAC);
      Result.MinHorizontalRecovery := definitions.SafeReal64
        (Msg.all.getMinHorizontalRecovery);
      Result.VerticalNMAC := definitions.SafeReal64 (Msg.all.getVerticalNMAC);
      Result.MinVerticalRecovery := definitions.SafeReal64 (Msg.all.getMinVerticalRecovery);
      Result.HorizontalContourThreshold := definitions.SafeReal64
        (Msg.all.getHorizontalContourThreshold);
      Result.DTHR := definitions.SafeReal64 (Msg.all.getDTHR);
      Result.ZTHR := definitions.SafeReal64 (Msg.all.getZTHR);
      Result.TTHR := definitions.SafeReal64 (Msg.all.getTTHR);
      Result.RTCAAlertLevels := Common.UInt32 (Msg.all.getRTCAAlertLevels);
      Result.AlertTime1 := definitions.ttlowc_sec (Msg.all.getAlertTime1);
      Result.EarlyAlertTime1 := definitions.ttlowc_sec (Msg.all.getEarlyAlertTime1);
      Result.AlertTime2 := definitions.ttlowc_sec (Msg.all.getAlertTime2);
      Result.EarlyAlertTime2 := definitions.ttlowc_sec (Msg.all.getEarlyAlertTime2);
      Result.AlertTime3 := definitions.ttlowc_sec (Msg.all.getAlertTime3);
      Result.EarlyAlertTime3 := definitions.ttlowc_sec (Msg.all.getEarlyAlertTime3);
      Result.HorizontalDetectionType := Msg.all.getHorizontalDetectionType;

      return Result;
   end As_DAIDALUSConfiguration_Message;

   --------------------------------------
   -- As_GroundHeadingInterval_Message --
   --------------------------------------

   function As_GroundHeadingInterval_Message
     (Msg : not null GroundHeadingInterval_Acc) return
     LMCP_Messages.Real64_Array
   is
      Result : LMCP_Messages.Real64_Array;
      use LMCP_Messages; use Common;
      GHInterval : constant larcfm.DAIDALUS.GroundHeadingInterval.Real64_2D
      := Msg.all.getGroundHeadings.all;
   begin
      Result := (Real64 (GHInterval (1)), Real64 (GHInterval (2)));
      return Result;
   end As_GroundHeadingInterval_Message;

   ------------------------------------
   -- As_GroundSpeedInterval_Message --
   ------------------------------------

   function As_GroundSpeedInterval_Message
     (Msg : not null GroundSpeedInterval_Acc) return
     LMCP_Messages.Real64_Array
   is
      Result : LMCP_Messages.Real64_Array;
      GSInterval : constant larcfm.DAIDALUS.GroundSpeedInterval.Real64_2D :=
        Msg.all.getGroundSpeeds.all;
      use LMCP_Messages; use Common;
   begin
      Result := (Real64 (GSInterval (1)), Real64 (GSInterval (2)));
      return Result;
   end As_GroundSpeedInterval_Message;

   -------------------------------------
   --As_VerticalSpeedInterval_Message --
   -------------------------------------

   function As_VerticalSpeedInterval_Message
     (Msg : not null VerticalSpeedInterval_Acc) return
     LMCP_Messages.Real64_Array
   is
      Result : LMCP_Messages.Real64_Array;
      VSInterval : constant larcfm.DAIDALUS.VerticalSpeedInterval.Real64_2D :=
        Msg.all.getVerticalSpeeds.all;
      use LMCP_Messages; use Common;
   begin
      Result := (Real64 (VSInterval (1)), Real64 (VSInterval (2)));
      return Result;
   end As_VerticalSpeedInterval_Message;

   ---------------------------------
   -- As_AltitudeInterval_Message --
   ---------------------------------

   function As_AltitudeInterval_Message
     (Msg : not null AltitudeInterval_Acc) return
     LMCP_Messages.Real32_Array
   is
      Result : LMCP_Messages.Real32_Array;
      AltInterval : constant larcfm.DAIDALUS.AltitudeInterval.Real64_2D :=
        Msg.all.getAltitude.all;
      use LMCP_Messages; use Common;
   begin
      Result := (Real32 (AltInterval (1)), Real32 (AltInterval (2)));
      return Result;
   end As_AltitudeInterval_Message;

   ----------------------------------------------
   -- As_GroundHeadingRecoveryInterval_Message --
   ----------------------------------------------

   function As_GroundHeadingRecoveryInterval_Message
     (Msg : not null GroundHeadingRecoveryInterval_Acc) return
     LMCP_Messages.Real64_Array
   is
      Result : LMCP_Messages.Real64_Array;
      GHRInterval : constant larcfm.DAIDALUS.GroundHeadingRecoveryInterval.
        Real64_2D := Msg.all.getRecoveryGroundHeadings.all;
      use LMCP_Messages; use Common;
   begin
      Result := (Real64 (GHRInterval (1)), Real64 (GHRInterval (2)));
      return Result;
   end As_GroundHeadingRecoveryInterval_Message;

   --------------------------------------------
   -- As_GroundSpeedRecoveryInterval_Message --
   --------------------------------------------

   function As_GroundSpeedRecoveryInterval_Message
     (Msg : not null GroundSpeedRecoveryInterval_Acc) return
     LMCP_Messages.Real64_Array
   is
      Result : LMCP_Messages.Real64_Array;
      GSRInterval : constant larcfm.DAIDALUS.GroundSpeedRecoveryInterval.
        Real64_2D := Msg.all.getRecoveryGroundSpeeds.all;
      use LMCP_Messages; use Common;
   begin
      Result := (Real64 (GSRInterval (1)), Real64 (GSRInterval (2)));
      return Result;
   end As_GroundSpeedRecoveryInterval_Message;

   ---------------------------------------------
   -- As VerticalSpeedRecoveryInterval_Message--
   ---------------------------------------------

   function As_VerticalSpeedRecoveryInterval_Message
     (Msg : not null VerticalSpeedRecoveryInterval_Acc) return
     LMCP_Messages.Real64_Array
   is
      Result : LMCP_Messages.Real64_Array;
      VSRInterval : constant larcfm.DAIDALUS.VerticalSpeedRecoveryInterval.
        Real64_2D := Msg.all.getRecoveryVerticalSpeed.all;
      use LMCP_Messages; use Common;
   begin
      Result := (Real64 (VSRInterval (1)), Real64 (VSRInterval (2)));
      return Result;
   end As_VerticalSpeedRecoveryInterval_Message;

   -----------------------------------------
   -- As_AltitudeRecoveryInterval_Message --
   -----------------------------------------

   function As_AltitudeRecoveryInterval_Message
     (Msg : not null AltitudeRecoveryInterval_Acc) return
     LMCP_Messages.Real32_Array
   is
      Result : LMCP_Messages.Real32_Array;
      ARInterval : constant larcfm.DAIDALUS.AltitudeRecoveryInterval.Real64_2D
        := Msg.all.getRecoveryAltitude.all;
      use LMCP_Messages; use Common;
   begin
      Result := (Real32 (ARInterval (1)), Real32 (ARInterval (2)));
      return Result;
   end As_AltitudeRecoveryInterval_Message;

   ---------------------------------------------
   -- As_WellClearViolationsIntervals_Message --
   ---------------------------------------------
   function As_WellClearViolationsIntervals_Message
     (Msg : not null WellClearViolationIntervals_Any)
      return LMCP_Messages.WellClearViolationIntervals
   is
      Result : LMCP_Messages.WellClearViolationIntervals;
      Temp : LMCP_Messages.Real64_Array;
      Test : LMCP_Messages.GroundHeading_Sequence;
      use Common;
   begin
      for Entity of Msg.all.getEntityList.all loop
         Result.EntityList := LMCP_Messages.IDType_sequences.Add
           (Result.EntityList, definitions.ID_Type (Entity));
      end loop;
      for ViolationTime of Msg.all.getTimeToViolationList.all loop
         Result.TimeToViolationList := LMCP_Messages.ttlowc_sequences.Add
           (Result.TimeToViolationList, definitions.ttlowc_sec (ViolationTime));
      end loop;
      for AlertLevel of Msg.all.getAlertLevelList.all loop
         Result.AlertLevelList := LMCP_Messages.Real64_sequences.Add (Result.AlertLevelList,
                                       Real64 (AlertLevel));
      end loop;
      Result.EntityID := definitions.ID_Type (Msg.all.getEntityId);
      Result.CurrentHeading := definitions.Heading_Type_deg
        (Msg.all.getCurrentHeading);
      Result.CurrentGroundSpeed := definitions.GroundSpeed_Type_mps
        (Msg.all.getCurrentGoundSpeed);
      Result.CurrentVerticalSpeed := definitions.VerticalSpeed_Type_mps
        (Msg.all.getCurrentVerticalSpeed);
      Result.CurrentAltitude := definitions.Altitude_Type_m
        (Msg.all.getCurrentAltitude);
      Result.CurrentLatitude := definitions.latitude_type_deg
        (Msg.all.getCurrentLatitude);
      Result.CurrentLongitude := definitions.longitude_type_deg
        (Msg.all.getCurrentLongitude);
      Result.CurrentTime := definitions.ttlowc_sec (Msg.all.getCurrentTime);
      for GroundHeadingInterval_Vector of Msg.getWCVGroundHeadingIntervals.
      all loop
         Temp := As_GroundHeadingInterval_Message
           (GroundHeadingInterval_Vector);
         Result.WCVGroundHeadingIntervals.GroundHeadings := LMCP_Messages.
           Generic_Real64_Sequences.Add
             (Result.WCVGroundHeadingIntervals.GroundHeadings,
                                       Temp);
      end loop;
      for GroundHeadingBandsRegion_vector of Msg.all.getWCVGroundHeadingRegions.
      all loop
         case GroundHeadingBandsRegion_vector is
            when larcfm.DAIDALUS.Enumerations.NEAR =>
               Result.WCVGroundHeadingRegions := LMCP_Messages.
                 BandsRegion_sequences.Add (Result.WCVGroundHeadingRegions,
                                            LMCP_Messages.NEAR);
            when larcfm.DAIDALUS.Enumerations.MID =>
               Result.WCVGroundHeadingRegions := LMCP_Messages.
                 BandsRegion_sequences.Add
                 (Result.WCVGroundHeadingRegions, LMCP_Messages.MID);
            when larcfm.DAIDALUS.Enumerations.FAR =>
               Result.WCVGroundHeadingRegions := LMCP_Messages.
                 BandsRegion_sequences.Add
                 (Result.WCVGroundHeadingRegions, LMCP_Messages.FAR);
         end case;
      end loop;
      for GroundSpeedInterval_Vector of Msg.all.getWCVGroundSpeedIntervals.all
      loop
         Result.WCVGroundSpeedIntervals.GroundSpeeds := LMCP_Messages.
           Generic_Real64_Sequences.Add
           (Result.WCVGroundSpeedIntervals.GroundSpeeds,
            As_GroundSpeedInterval_Message (GroundSpeedInterval_Vector));
      end loop;
      for GroundSpeedBandsRegion_Vector of Msg.all.getWCVGroundSpeedRegions.all
      loop
         case GroundSpeedBandsRegion_Vector is
            when larcfm.DAIDALUS.Enumerations.NEAR =>
               Result.WCVGroundSpeedRegions := LMCP_Messages.
                 BandsRegion_sequences.Add
                 (Result.WCVGroundSpeedRegions, LMCP_Messages.NEAR);
            when larcfm.DAIDALUS.Enumerations.MID =>
               Result.WCVGroundSpeedRegions := LMCP_Messages.
                 BandsRegion_sequences.Add
                 (Result.WCVGroundSpeedRegions, LMCP_Messages.MID);
            when larcfm.DAIDALUS.Enumerations.FAR =>
               Result.WCVGroundSpeedRegions := LMCP_Messages.
                 BandsRegion_sequences.Add
                 (Result.WCVGroundSpeedRegions, LMCP_Messages.FAR);
         end case;
      end loop;
      for VerticalSpeedInterval_Vector of Msg.all.getWCVVerticalSpeedIntervals.
      all loop
         Result.WCVVerticalSpeedIntervals.VerticalSpeeds := LMCP_Messages.
           Generic_Real64_Sequences.Add
           (Result.WCVVerticalSpeedIntervals.VerticalSpeeds,
            As_VerticalSpeedInterval_Message (VerticalSpeedInterval_Vector));
      end loop;
      for VerticalSpeedBandsRegion_Vector of Msg.all.getWCVVerticalSpeedRegions.
      all loop
         case VerticalSpeedBandsRegion_Vector is
            when larcfm.DAIDALUS.Enumerations.NEAR =>
               Result.WCVVerticalSpeedRegions := LMCP_Messages.
                 BandsRegion_sequences.Add
                 (Result.WCVVerticalSpeedRegions, LMCP_Messages.NEAR);
            when larcfm.DAIDALUS.Enumerations.MID =>
               Result.WCVVerticalSpeedRegions := LMCP_Messages.
                 BandsRegion_sequences.Add
                 (Result.WCVVerticalSpeedRegions, LMCP_Messages.MID);
            when larcfm.DAIDALUS.Enumerations.FAR =>
               Result.WCVVerticalSpeedRegions := LMCP_Messages.
                 BandsRegion_sequences.Add
                 (Result.WCVVerticalSpeedRegions, LMCP_Messages.FAR);
         end case;
      end loop;
      for AltitudeInterval_Vector of Msg.all.getWCVAlitudeIntervals.all loop
         Result.WCVAltitudeIntervals.Altitude := LMCP_Messages.
           Generic_Real32_Sequences.Add
           (Result.WCVAltitudeIntervals.Altitude,
            As_AltitudeInterval_Message (AltitudeInterval_Vector));
      end loop;
      for AltitudeBandsRegion_Vector of Msg.all.getWCVAltitudeRegions.all loop
         case AltitudeBandsRegion_Vector is
            when larcfm.DAIDALUS.Enumerations.NEAR =>
               Result.WCVAltitudeRegions := LMCP_Messages.
                 BandsRegion_sequences.Add
                 (Result.WCVAltitudeRegions, LMCP_Messages.NEAR);
            when larcfm.DAIDALUS.Enumerations.MID =>
               Result.WCVAltitudeRegions := LMCP_Messages.
                 BandsRegion_sequences.Add
                 (Result.WCVAltitudeRegions, LMCP_Messages.MID);
            when larcfm.DAIDALUS.Enumerations.FAR =>
               Result.WCVAltitudeRegions := LMCP_Messages.
                 BandsRegion_sequences.Add
                 (Result.WCVAltitudeRegions, LMCP_Messages.FAR);
         end case;
      end loop;
      for RecoveryGroundHeadingInterval_Vector of Msg.all.
        getRecoveryGroundHeadingIntervals.all loop
         Result.RecoveryGroundHeadingIntervals.RecoveryGroundHeadings :=
           LMCP_Messages.Generic_Real64_Sequences.Add
             (Result.RecoveryGroundHeadingIntervals.RecoveryGroundHeadings,
                              As_GroundHeadingRecoveryInterval_Message
                                (RecoveryGroundHeadingInterval_Vector));
      end loop;
      for RecoveryGroundSpeedInterval_Vector of Msg.all.
        getRecoveryGroundSpeedIntervals.all loop
         Result.RecoveryGroundSpeedIntervals.RecoveryGroundSpeeds :=
           LMCP_Messages.Generic_Real64_Sequences.Add
             (Result.RecoveryGroundSpeedIntervals.RecoveryGroundSpeeds,
                              As_GroundSpeedRecoveryInterval_Message
                                (RecoveryGroundSpeedInterval_Vector));
      end loop;
      for RecoveryVerticalSpeedInterval_Vector of Msg.all.
        getRecoveryVerticalSpeedIntervals.all loop
         Result.RecovertyVerticalSpeedIntervals.RecoveryVerticalSpeed :=
           LMCP_Messages.Generic_Real64_Sequences.Add
             (Result.RecovertyVerticalSpeedIntervals.RecoveryVerticalSpeed,
                              As_VerticalSpeedRecoveryInterval_Message
                                (RecoveryVerticalSpeedInterval_Vector));
      end loop;
      for RecoveryAltitudeInterval_Vector of Msg.all.
        getRecoveryAltitudeIntervals.all loop
         Result.RecoveryAltitudeIntervals.RecoveryAltitude := LMCP_Messages.
           Generic_Real32_Sequences.Add
           (Result.RecoveryAltitudeIntervals.RecoveryAltitude,
            As_AltitudeRecoveryInterval_Message
              (RecoveryAltitudeInterval_Vector));
      end loop;

      return Result;
   end As_WellClearViolationsIntervals_Message;

end LMCP_Message_Conversions;
