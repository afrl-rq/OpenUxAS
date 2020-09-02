with Common;
with AFRL.CMASI.Enumerations;
with Afrl.Cmasi.AutomationRequest;
with AVTAS.LMCP.Types;
with UxAS.Messages.Route.RouteResponse;            use UxAS.Messages.Route.RouteResponse;
with uxas.messages.lmcptask.AssignmentCostMatrix;  use uxas.messages.lmcptask.AssignmentCostMatrix;
with uxas.messages.lmcptask.TaskOptionCost;        use uxas.messages.lmcptask.TaskOptionCost;
with uxas.messages.lmcptask.TaskAssignmentSummary; use uxas.messages.lmcptask.TaskAssignmentSummary;
with uxas.messages.lmcptask.TaskAssignment;        use uxas.messages.lmcptask.TaskAssignment;

package body LMCP_Message_Conversions is

   -------------------------------------
   -- As_AssignmentCostMatrix_Message --
   -------------------------------------

   function As_AssignmentCostMatrix_Message
     (msg : not null AssignmentCostMatrix_Any)
      return LMCP_Messages.AssignmentCostMatrix
   is

      function As_TaskOptionCost
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
         Result.timeToGo := Common.Int64 (Arg.getTimeToGo);
         return Result;
      end As_TaskOptionCost;

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
         Result.CostMatrix := Add (Result.CostMatrix, As_TaskOptionCost (TaskOptionCost));
      end loop;
      return Result;
   end As_AssignmentCostMatrix_Message;

   function As_RouteConstraints_Acc (Msg : LMCP_Messages.RouteConstraints) return RouteConstraints_Acc;

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

   ----------------------
   -- As_VehicleAction --
   ----------------------

   function As_VehicleAction_Acc (Msg : LMCP_Messages.VehicleAction) return VehicleAction_Acc is
      Result : constant VehicleAction_Acc := new VehicleAction;
   begin
      for Id : Common.Int64 of Msg.AssociatedTaskList loop
         Result.getAssociatedTaskList.Append (AVTAS.LMCP.Types.int64 (Id));
      end loop;
      return result;
   end As_VehicleAction_Acc;

   -----------------------------
   -- As_KeyValuePair_Message --
   -----------------------------

   function As_KeyValuePair_Message
     (Msg : not null KeyValuePair_Acc)
   return LMCP_Messages.KeyValuePair
   is
      Result : LMCP_Messages.KeyValuePair;
   begin
      Result.Key := Msg.GetKey;
      Result.Value := Msg.getValue;
      return Result;
   end As_KeyValuePair_Message;

   -------------------------
   -- As_KeyValuePair_Acc --
   -------------------------

   function As_KeyValuePair_Acc (Msg : LMCP_Messages.KeyValuePair) return KeyValuePair_Acc is
      Result : constant KeyValuePair_Acc := new KeyValuePair;
   begin
      Result.SetKey (Msg.Key);
      Result.setValue (Msg.Value);
      return Result;
   end As_KeyValuePair_Acc;

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
      Result.Number := Common.Int64 (Msg.GetNumber);
      Result.NextWaypoint := Common.Int64 (Msg.getNextWaypoint);
      Result.Speed := Common.Real32 (Msg.GetSpeed);

      case Msg.GetSpeedType is
         when Afrl.Cmasi.Enumerations.Airspeed    => Result.SpeedType := LMCP_Messages.Airspeed;
         when Afrl.Cmasi.Enumerations.Groundspeed => Result.SpeedType := LMCP_Messages.Groundspeed;
      end case;

      Result.ClimbRate := Common.Real32 (Msg.GetClimbRate);

      case Msg.GetTurnType is
         when Afrl.Cmasi.Enumerations.TurnShort => Result.TurnType := LMCP_Messages.TurnShort;
         when Afrl.Cmasi.Enumerations.FlyOver   => Result.TurnType := LMCP_Messages.FlyOver;
      end case;

      for VA of Msg.getVehicleActionList.all loop
         Result.VehicleActionList := LMCP_Messages.Add (Result.VehicleActionList, As_VehicleAction_Message (VA));
      end loop;

      Result.ContingencyWaypointA := Common.Int64 (Msg.GetContingencyWaypointA);
      Result.ContingencyWaypointB := Common.Int64 (Msg.GetContingencyWaypointB);

      for Id of Msg.GetAssociatedTasks.all loop
        Result.AssociatedTasks := Common.Add (Result.AssociatedTasks, Common.Int64 (Id));
      end loop;

      return Result;
   end As_Waypoint_Message;

   ---------------------
   -- As_Waypoint_Acc --
   ---------------------

   function As_Waypoint_Acc (Msg : LMCP_Messages.Waypoint) return Waypoint_Acc is
      Result : constant WayPoint_Acc := new AFRL.CMASI.Waypoint.Waypoint;
   begin
      --  the Location3D components
      Result.SetLatitude (AVTAS.LMCP.Types.Real64 (Msg.Latitude));
      Result.SetLongitude (AVTAS.LMCP.Types.Real64 (Msg.Longitude));
      Result.SetAltitude (AVTAS.LMCP.Types.Real32 (Msg.Altitude));
      case Msg.AltitudeType is
         when LMCP_Messages.AGL => Result.setAltitudeType (AFRL.CMASI.Enumerations.AGL);
         when LMCP_Messages.MSL => Result.setAltitudeType (AFRL.CMASI.Enumerations.MSL);
      end case;

      --  the waypoint extensions

      Result.setNumber (AVTAS.LMCP.Types.Int64 (Msg.Number));
      Result.setNextWaypoint (AVTAS.LMCP.Types.Int64 (Msg.NextWaypoint));
      Result.SetSpeed (AVTAS.LMCP.Types.Real32 (Msg.Speed));

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
      Result.SetContingencyWaypointB (AVTAS.LMCP.Types.Int64 (Msg.ContingencyWaypointB));

      for Id of Msg.AssociatedTasks loop
         Result.getAssociatedTasks.Append (AVTAS.LMCP.Types.Int64 (Id));
      end loop;

      return Result;
   end As_Waypoint_Acc;

   ---------------------------
   -- As_Location3D_Message --
   ---------------------------

   function As_Location3D_Message
     (Msg : not null Location3D_Any)
   return LMCP_Messages.Location3D
   is
      Result : LMCP_Messages.Location3D;
   begin
      Result.Latitude := Common.Real64 (Msg.GetLatitude);
      Result.Longitude := Common.Real64 (Msg.GetLongitude);
      Result.Altitude := Common.Real32 (Msg.GetAltitude);
      --  For this enumeration type component we could use 'Val and 'Pos to
      --  convert the values, but that would not be robust in the face of
      --  independent changes to either one of the two enumeration type
      --  decls, especially the order. Therefore we do an explicit comparison.
      case Msg.GetAltitudeType is
         when Afrl.Cmasi.Enumerations.AGL => Result.AltitudeType := LMCP_Messages.AGL;
         when Afrl.Cmasi.Enumerations.MSL => Result.AltitudeType := LMCP_Messages.MSL;
      end case;
      return Result;
   end As_Location3D_Message;

   -----------------------
   -- As_Location3D_Any --
   -----------------------

   function As_Location3D_Any (Msg : LMCP_Messages.Location3D) return Location3D_Any is
      Result : constant Location3D_Acc := new Location3D;
   begin
      Result.setLatitude (AVTAS.LMCP.Types.Real64 (Msg.Latitude));
      Result.setLongitude (AVTAS.LMCP.Types.Real64 (Msg.Longitude));
      Result.setAltitude (AVTAS.LMCP.Types.Real32 (Msg.Altitude));
      case Msg.AltitudeType is
         when LMCP_Messages.AGL => Result.SetAltitudeType (AFRL.CMASI.Enumerations.AGL);
         when LMCP_Messages.MSL => Result.SetAltitudeType (AFRL.CMASI.Enumerations.MSL);
      end case;

      return Location3D_Any (Result);
   end As_Location3D_Any;

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
      for WP of Msg.GetWaypoints.all loop
         Result.Waypoints := Add (Result.Waypoints, As_Waypoint_Message (WP));
      end loop;
      Result.RouteCost := Common.Int64 (Msg.GetRouteCost);
      for Error of Msg.GetRouteError.all loop
         Result.RouteError := Add (Result.RouteError, As_KeyValuePair_Message (Error));
      end loop;
      return Result;
   end As_RoutePlan_Message;

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
      Result.ResponseID       := Int64 (Msg.GetResponseID);
      Result.AssociatedTaskID := Int64 (Msg.GetAssociatedTaskID);
      Result.VehicleID        := Int64 (Msg.GetVehicleID);
      Result.OperatingRegion  := Int64 (Msg.GetOperatingRegion);

      for Plan of Msg.getRouteResponses.all loop
         New_RoutePlan.RouteID   := Int64 (Plan.GetRouteID);
         New_RoutePlan.RouteCost := Int64 (Plan.GetRouteCost);

         for WP of Plan.GetWaypoints.all loop
            New_RoutePlan.Waypoints := Add (New_RoutePlan.Waypoints, As_Waypoint_Message (WP));
         end loop;

         for Error of Plan.GetRouteError.all loop
            New_RoutePlan.RouteError := Add (New_RoutePlan.RouteError, As_KeyValuePair_Message (Error));
         end loop;

         Result.RouteResponses := Add (Result.RouteResponses, New_RoutePlan);
      end loop;

      return Result;
   end As_RoutePlanResponse_Message;

   ------------------------------
   -- As_RoutePlanResponse_Acc --
   ------------------------------

   function As_RoutePlanResponse_Acc (Msg : LMCP_Messages.RoutePlanResponse'Class) return RoutePlanResponse_Acc is
      Result         : constant RoutePlanResponse_Acc := new RoutePlanResponse;
      New_Route_Plan : Uxas.Messages.Route.RoutePlan.RoutePlan_Acc;
   begin
      Result.SetResponseID (AVTAS.LMCP.Types.Int64 (Msg.ResponseID));
      Result.SetAssociatedTaskID (AVTAS.LMCP.Types.Int64 (Msg.AssociatedTaskID));
      Result.setVehicleID (AVTAS.LMCP.Types.Int64 (Msg.VehicleID));
      Result.SetOperatingRegion (AVTAS.LMCP.Types.Int64 (Msg.OperatingRegion));

      for Plan_Msg : LMCP_Messages.RoutePlan of Msg.RouteResponses loop
         New_Route_Plan := new Uxas.Messages.Route.RoutePlan.RoutePlan;

         New_Route_Plan.SetRouteID (AVTAS.LMCP.Types.Int64 (Plan_Msg.RouteID));
         New_Route_Plan.SetRouteCost (AVTAS.LMCP.Types.Int64 (Plan_Msg.RouteCost));

         --  waypoints...
         for WP : LMCP_Messages.Waypoint of Plan_Msg.Waypoints loop
            New_Route_Plan.GetWaypoints.Append (Waypoint_Any (As_Waypoint_Acc (WP)));
         end loop;

         --  route errors...
         for KVP : LMCP_Messages.KeyValuePair of Plan_Msg.RouteError loop
            New_Route_Plan.GetRouteError.Append (As_KeyValuePair_Acc (KVP));
         end loop;

         Result.getRouteResponses.Append (New_Route_Plan);
      end loop;

      return Result;
   end As_RoutePlanResponse_Acc;

   -----------------------------
   -- As_RouteRequest_Message --
   -----------------------------

   function As_RouteRequest_Message
     (Msg : not null RouteRequest_Any)
   return LMCP_Messages.RouteRequest
   is
      Result : LMCP_Messages.RouteRequest;
   begin
      Result.RequestID        := Common.Int64 (Msg.GetRequestID);
      Result.AssociatedTaskID := Common.Int64 (Msg.GetAssociatedTaskID);
      for VID of Msg.getVehicleID.all loop
        Result.VehicleID := Common.Add (Result.VehicleID, Common.Int64 (VID));
      end loop;
      Result.OperatingRegion   := Common.Int64 (Msg.GetOperatingRegion);
      Result.IsCostOnlyRequest := Msg.GetIsCostOnlyRequest;
      for RC of Msg.getRouteRequests.all loop
        Result.RouteRequests := LMCP_Messages.Add (Result.RouteRequests, As_RouteConstraints_Message (RouteConstraints_Any (RC)));
      end loop;

      return Result;
   end As_RouteRequest_Message;

   -------------------------
   -- As_RouteRequest_Acc --
   -------------------------

   function As_RouteRequest_Acc (Msg : LMCP_Messages.RouteRequest'Class) return RouteRequest_Acc is
      Result : constant RouteRequest_Acc := new RouteRequest;
   begin
      Result.setRequestID (AVTAS.LMCP.Types.Int64 (Msg.RequestID));
      Result.setAssociatedTaskID (AVTAS.LMCP.Types.Int64 (Msg.AssociatedTaskID));
      for VID of Msg.VehicleID loop
         Result.getVehicleID.Append (AVTAS.LMCP.Types.Int64 (VID));
      end loop;
      Result.setOperatingRegion (AVTAS.LMCP.Types.Int64 (Msg.OperatingRegion));
      Result.SetIsCostOnlyRequest (Msg.IsCostOnlyRequest);
      for RC of Msg.RouteRequests loop
         Result.getRouteRequests.Append (As_RouteConstraints_Acc (RC));
      end loop;

      return Result;
   end As_RouteRequest_Acc;

   ---------------------------------
   -- As_RouteConstraints_Message --
   ---------------------------------

   function As_RouteConstraints_Message
     (Msg : not null RouteConstraints_Any)
   return LMCP_Messages.RouteConstraints
   is
      Result : LMCP_Messages.RouteConstraints;
   begin
      Result.RouteID         := Common.Int64 (Msg.GetRouteID);
      Result.StartLocation   := As_Location3D_Message (Msg.GetStartLocation);
      Result.StartHeading    := Common.Real32 (Msg.GetStartHeading);
      Result.UseStartHeading := Msg.GetUseStartHeading;
      Result.EndLocation     := As_Location3D_Message (Msg.GetEndLocation);
      Result.EndHeading      := Common.Real32 (Msg.GetEndHeading);
      Result.UseEndHeading   := Msg.GetUseEndHeading;
      return Result;
   end As_RouteConstraints_Message;

   -----------------------------
   -- As_RouteConstraints_Acc --
   -----------------------------

   function As_RouteConstraints_Acc (Msg : LMCP_Messages.RouteConstraints) return RouteConstraints_Acc is
      Result : constant RouteConstraints_Acc := new RouteConstraints;
   begin
      Result.SetRouteID (AVTAS.LMCP.Types.Int64 (Msg.RouteID));
      Result.setStartLocation (As_Location3D_Any (Msg.StartLocation));
      Result.setStartHeading (AVTAS.LMCP.Types.Real32 (Msg.StartHeading));
      Result.setUseStartHeading (Msg.UseStartHeading);
      Result.setEndLocation (As_Location3D_Any (Msg.EndLocation));
      Result.setEndHeading (AVTAS.LMCP.Types.Real32 (Msg.EndHeading));
      Result.setUseEndHeading (Msg.UseEndHeading);
      return Result;
   end As_RouteConstraints_Acc;

   ---------------------------------
   -- As_RoutePlanRequest_Message --
   ---------------------------------

   function As_RoutePlanRequest_Message
     (Msg : not null RoutePlanRequest_Any)
   return LMCP_Messages.RoutePlanRequest
   is
      Result : LMCP_Messages.RoutePlanRequest;
   begin
      Result.RequestID         := Common.Int64 (Msg.GetRequestID);
      Result.AssociatedTaskID  := Common.Int64 (Msg.GetAssociatedTaskID);
      Result.VehicleID         := Common.Int64 (Msg.GetVehicleID);
      Result.OperatingRegion   := Common.Int64 (Msg.GetOperatingRegion);
      Result.IsCostOnlyRequest := Msg.GetIsCostOnlyRequest;
      for RC of Msg.getRouteRequests.all loop
        Result.RouteRequests := LMCP_Messages.Add (Result.RouteRequests, As_RouteConstraints_Message (RouteConstraints_Any (RC)));
      end loop;
      return Result;
   end As_RoutePlanRequest_Message;

   ------------------------------
   -- As_RoutePlanResponse_Acc --
   ------------------------------

   function As_RoutePlanRequest_Acc (Msg : LMCP_Messages.RoutePlanRequest'Class) return RoutePlanRequest_Acc is
      Result : constant RoutePlanRequest_Acc := new RoutePlanRequest;
   begin
      Result.SetRequestID (AVTAS.LMCP.Types.Int64 (Msg.RequestID));
      Result.SetAssociatedTaskID (AVTAS.LMCP.Types.Int64 (Msg.AssociatedTaskID));
      Result.SetVehicleID (AVTAS.LMCP.Types.Int64 (Msg.VehicleID));
      Result.SetOperatingRegion (AVTAS.LMCP.Types.Int64 (Msg.OperatingRegion));
      Result.setIsCostOnlyRequest (Msg.IsCostOnlyRequest);
      for RC : LMCP_Messages.RouteConstraints of Msg.RouteRequests loop
         Result.GetRouteRequests.Append (As_RouteConstraints_Acc (RC));
      end loop;
      return Result;
   end As_RoutePlanRequest_Acc;

   --------------------------
   -- As_RouteResponse_Acc --
   --------------------------

   function As_RouteResponse_Acc (Msg : LMCP_Messages.RouteResponse'Class) return RouteResponse_Acc is
      Result : constant RouteResponse_Acc := new RouteResponse;
   begin
      Result.SetResponseID (AVTAS.LMCP.Types.Int64 (Msg.ResponseID));
      for RP : LMCP_Messages.RoutePlanResponse of Msg.Routes loop
         Result.getRoutes.Append (As_RoutePlanResponse_Acc (RP));
      end loop;

      return Result;
   end As_RouteResponse_Acc;

   ----------------------------------------
   -- As_UniqueAutomationRequest_Message --
   ----------------------------------------

   function As_UniqueAutomationRequest_Message
     (Msg : not null UniqueAutomationRequest_Any)
      return LMCP_Messages.UniqueAutomationRequest is
      Result : LMCP_Messages.UniqueAutomationRequest;
      use Common;
      use all type LMCP_Messages.PlanningState_Seq;
   begin
      Result.RequestID := Int64 (Msg.all.getRequestID);

      for EntityId of Msg.all.getOriginalRequest.getEntityList.all loop
         Result.EntityList := Add (Result.EntityList, int64 (EntityId));
      end loop;

      Result.OperatingRegion := Int64 (Msg.all.getOriginalRequest.getOperatingRegion);

      for MsgPlanningState of Msg.all.getPlanningStates.all loop
         declare
            PlanningState : LMCP_Messages.PlanningState;
         begin
            PlanningState.EntityId := Int64 (MsgPlanningState.all.getEntityId);
            PlanningState.PlanningPosition := As_Location3D_Message (MsgPlanningState.all.getPlanningPosition);
            PlanningState.PlanningHeading := Real32 (MsgPlanningState.all.getPlanningHeading);

            Result.PlanningStates := Add (Result.PlanningStates, PlanningState);
         end;
      end loop;

      for TaskId of Msg.all.getOriginalRequest.getTaskList.all loop
         Result.TaskList := Add (Result.TaskList, Int64 (TaskId));
      end loop;

      Result.TaskRelationships := Msg.all.getOriginalRequest.getTaskRelationships;

      return Result;

   end As_UniqueAutomationRequest_Message;

   function As_TaskAssignment_Acc
     (Msg : LMCP_Messages.TaskAssignment)
      return TaskAssignment_Acc
   is
      Result : TaskAssignment_Acc := new TaskAssignment;
      use Avtas.lmcp.types;
   begin
      Result.setTaskId (Int64 (Msg.TaskID));
      Result.setOptionId (Int64 (Msg.OptionId));
      Result.setAssignedVehicle (Int64 (Msg.AssignedVehicle));
      Result.setTimeThreshold (Int64 (Msg.TimeThreshold));
      Result.setTimeTaskCompleted (Int64 (Msg.TimeTaskCompleted));

      return Result;
   end As_TaskAssignment_Acc;

   function As_TaskAssignmentSummary_Acc
     (Msg : LMCP_Messages.TaskAssignmentSummary'Class)
      return TaskAssignmentSummary_Acc
   is
      Result : TaskAssignmentSummary_Acc := new TaskAssignmentSummary;
      use Avtas.lmcp.types;
   begin
      Result.setCorrespondingAutomationRequestID (Int64 (Msg.CorrespondingAutomationRequestID));
      Result.setOperatingRegion (Int64 (Msg.OperatingRegion));

      for TaskAssignment of Msg.TaskList loop
         Result.getTaskList.Append (As_TaskAssignment_Acc (TaskAssignment));
      end loop;
      return Result;
   end As_TaskAssignmentSummary_Acc;

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
        Int64 (Msg.getTaskId);
      Result.Composition := Msg.getComposition;

      for MsgOption of Msg.getOptions.all loop
         declare
            Option : LMCP_Messages.TaskOption;
         begin
            Option.TaskID := Int64 (MsgOption.all.getTaskId);
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

   ---------------------------
   -- As_TaskOptionCost_Acc --
   ---------------------------

   function As_TaskOptionCost_Acc
     (Msg : LMCP_Messages.TaskOptionCost)
      return TaskOptionCost_Acc
   is
      Result : constant TaskOptionCost_Acc := new TaskOptionCost;
      use Avtas.lmcp.types;
   begin
      Result.all.setVehicleID (Int64 (Msg.VehicleID));
      Result.all.setIntialTaskID (Int64 (Msg.InitialTaskID));
      Result.all.setIntialTaskOption (Int64 (Msg.InitialTaskOption));
      Result.all.setDestinationTaskID (Int64 (Msg.DestinationTaskID));
      Result.all.setDestinationTaskOption (Int64 (Msg.DestinationTaskOption));
      Result.all.setTimeToGo (Int64 (Msg.TimeToGo));
      return Result;
   end As_TaskOptionCost_Acc;

   ---------------------------------
   -- As_AssignmentCostMatrix_Acc --
   ---------------------------------

   function As_AssignmentCostMatrix_Acc (Msg : LMCP_Messages.AssignmentCostMatrix'Class)
                                         return AssignmentCostMatrix_Acc
   is
      Result : constant AssignmentCostMatrix_Acc := new AssignmentCostMatrix;
      use Avtas.lmcp.types;
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

   ----------------------------
   -- As_EntityState_Message --
   ----------------------------

   function As_EntityState_Message (Msg : not null EntityState_Any)
                                    return LMCP_Messages.EntityState
   is
      Result : LMCP_Messages.EntityState;
      use Common;
   begin
      Result.Id := Int64 (Msg.getID);
      Result.Location := As_Location3D_Message (Msg.getLocation);
      Result.Heading := Real32 (Msg.getHeading);

      return Result;
   end As_EntityState_Message;

   -------------------
   -- As_Object_Any --
   -------------------

   function As_Object_Any (Msg : LMCP_Messages.Message_Root'Class) return AVTAS.LMCP.Object.Object_Any is
      Result : AVTAS.LMCP.Object.Object_Any;
   begin
      --  TODO: Consider using the stream 'Write routines (not the 'Output
      --  versions) to write the message objects to a byte array, then use
      --  Unpack to get the LMCP pointer type from that. We'd need a function
      --  mapping Message_Root tags to the LMCP enumeration identifying message
      --  types (which handles the necessary ommision of writing the tags)

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
      else
         raise Program_Error with "unexpected message kind in Route_Aggregator_Message_Conversions.As_Object_Any";
         --  UniqueAutomationRequest is in the class but not sent
      end if;

      return Result;
   end As_Object_Any;

end LMCP_Message_Conversions;
