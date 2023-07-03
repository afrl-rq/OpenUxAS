with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Unit_Conversion_Utilities;
with Common; use Common;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with SPARK.Big_Integers; use SPARK.Big_Integers;
with SPARK.Containers.Types;               use SPARK.Containers.Types;

package body Plan_Builder with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   pragma Assertion_Policy (Ignore);

   --  Subprograms wraping insertion and deletion in m_pendingRoute. They
   --  restate part of the postcondition of the callee, but also reestablish
   --  the predicate of Route_Aggregator_State and compute the effect of the
   --  modification on Plan_To_Route.

   -----------------------
   -- Local subprograms --
   -----------------------
   package My_Conversions is new Unit_Conversion_Utilities (Real64);
   use My_Conversions;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions (Real64);
   use Functions;

   -------------------------------------
   -- Process_Task_Assignment_Summary --
   -------------------------------------
   procedure Process_Task_Assignment_Summary
     (State            : in out Plan_Builder_State;
      Config           : in out Plan_Builder_Configuration_Data;
      Mailbox          : in out Plan_Builder_Mailbox;
      Received_Message : LMCP_Messages.TaskAssignmentSummary)
   is
      Request_Id : Int64 := Received_Message.CorrespondingAutomationRequestID;
      Corresponding_Automation_Request : UniqueAutomationRequest;
      In_Progress_Response : UniqueAutomationResponse;
      Planning_State : LMCP_Messages.PlanningState;
      North_M : Real64;
      East_M : Real64;
      Latitude_Deg : Real64;
      Longitude_Deg : Real64;
      Projected_State : ProjectedState;
      Converter : Unit_Converter := (m_dLatitudeInitial_rad => 0.0, m_dLongitudeInitial_rad => 0.0, m_dRadiusMeridional_m => 0.0, m_dRadiusTransverse_m => 0.0, m_dRadiusSmallCircleLatitude_m => 0.0);
   begin

      if Contains (State.m_uniqueAutomationRequests, Request_Id) then
         Corresponding_Automation_Request := Element (State.m_uniqueAutomationRequests, Request_Id);
      else
         declare
            Message : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String ("ERROR::processTaskAssignmentSummary: Corresponding Unique Automation Request ID [" & Request_Id'Image & "] not found!");
         begin
            sendErrorMessage (Mailbox, Message);
         end;
         return;
      end if;

      if Length (Received_Message.TaskList) = 0 then
         declare
            Message : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String ("No assignments found for request " & Request_Id'Image);
         begin
            sendErrorMessage (Mailbox, Message);
         end;
         return;
      end if;

      for VehicleId of Corresponding_Automation_Request.EntityList loop
         if not Has_Key (State.m_currentEntityStates, VehicleId) then
            declare
               Message : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String ("ERROR::processTaskAssignmentSummary: Corresponding Unique Automation Request included vehicle ID [" & VehicleId'Image & "] which does not have a corresponding current state!");
            begin
               sendErrorMessage (Mailbox, Message);
            end;
            return;
         end if;
      end loop;

      Insert (State.m_assignmentSummaries, Request_Id, Received_Message);

      In_Progress_Response.ResponseID := Request_Id;
      Insert (State.m_inProgressResponse, Request_Id, In_Progress_Response);

      if Length (Corresponding_Automation_Request.EntityList) = 0 then
         for S of State.m_currentEntityStates loop
            Corresponding_Automation_Request.EntityList := Add (Corresponding_Automation_Request.EntityList, S);
         end loop;
      end if;

      declare
         Projected_States : ProjectedState_Seq;
      begin
         State.m_projectedEntityStates := Add(State.m_projectedEntityStates, Request_Id, Projected_States);
      end;

      for VehicleID of Corresponding_Automation_Request.EntityList loop
         declare
            Found_State : Boolean := False;
         begin
            for P in 1 .. Last (Corresponding_Automation_Request.PlanningStates) loop
               pragma Loop_Invariant (Length (Get (State.m_projectedEntityStates, Request_Id)) = To_Big_Integer(P - 1)
               and Has_Key (State.m_projectedEntityStates, Request_Id));
               if Get (Corresponding_Automation_Request.PlanningStates, P).EntityID = VehicleID then
                  Found_State := True;
                  Projected_State.State := Get (Corresponding_Automation_Request.PlanningStates, P);
               end if;
               if not Found_State then
                  Planning_State.EntityID := VehicleID;
                  Planning_State.PlanningPosition := Get (State.m_currentEntityStates, VehicleID).Location;
                  Planning_State.PlanningHeading := Get (State.m_currentEntityStates, VehicleID).Heading;
                  Convert_LatLong_Degrees_To_NorthEast_Meters (Converter, Get (State.m_currentEntityStates, VehicleID).Location.Latitude, Get (State.m_currentEntityStates, VehicleID).Location.Longitude, North_M, East_M);
                  North_M := North_M + Config.m_assignmentStartPointLead_m * (Cos (Real64 (Get (State.m_currentEntityStates, VehicleID).Heading)) * dDegreesToRadians);
                  East_M := East_M + Config.m_assignmentStartPointLead_m * (Sin (Real64 (Get (State.m_currentEntityStates, VehicleID).Heading)) * dDegreesToRadians);
                  Convert_NorthEast_Meters_To_LatLong_Degrees (Converter, North_M, East_M, Latitude_Deg, Longitude_Deg);
                  Planning_State.PlanningPosition.Latitude := Latitude_Deg;
                  Planning_State.PlanningPosition.Longitude := Longitude_Deg;
                  Projected_State.State := Planning_State;
                  Projected_State.Time := Get (State.m_currentEntityStates, VehicleID).Time;
               end if;
               State.m_projectedEntityStates := Set (State.m_projectedEntityStates,
                     Request_Id,
                     Add (Get (State.m_projectedEntityStates, Request_Id), Projected_State));
            end loop;
         end;
      end loop;
      Insert (State.m_remainingAssignments, Request_Id, Received_Message.TaskList);

      Send_Next_Task_Implementation_Request (Request_Id, Mailbox, State, Config);
   end Process_Task_Assignment_Summary;

   procedure Send_Next_Task_Implementation_Request
     (Unique_Request_Id : Int64;
      Mailbox          : in out Plan_Builder_Mailbox;
      State : in out Plan_Builder_State;
      Config : in out Plan_Builder_Configuration_Data)
   is
   begin
      if Contains (State.m_uniqueAutomationRequests, Unique_Request_Id) then
         if Length (Element (State.m_remainingAssignments, Unique_Request_Id)) = 0 then
            return;
         end if;

         declare
            taskAssignment : LMCP_Messages.TaskAssignment := Get (Element (State.m_remainingAssignments, Unique_Request_Id), TaskAssignment_Sequences.First);
            taskImplementationRequest : LMCP_Messages.TaskImplementationRequest;
            planState : ProjectedState;
         begin
            for PE of Int64_PS_Maps.Get (State.m_projectedEntityStates, Unique_Request_Id) loop
               if PE.State.EntityID = taskAssignment.AssignedVehicle then
                  planState := PE;
                  exit;
               end if;
            end loop;

            taskImplementationRequest.CorrespondingAutomationRequestID := Unique_Request_Id;
            taskImplementationRequest.RequestID := Config.m_taskImplementationId;
            Config.m_taskImplementationId := Config.m_taskImplementationId + 1;
            taskImplementationRequest.StartingWaypointID := planState.FinalWaypointID + 1;
            taskImplementationRequest.VehicleID := taskAssignment.AssignedVehicle;
            taskImplementationRequest.TaskID := taskAssignment.TaskID;
            taskImplementationRequest.OptionID := taskAssignment.OptionID;
            taskImplementationRequest.RegionID := Element (State.m_uniqueAutomationRequests, Unique_Request_Id).OperatingRegion;
            taskImplementationRequest.TimeThreshold := taskAssignment.TimeThreshold;
            taskImplementationRequest.StartHeading := planState.State.PlanningHeading;
            taskImplementationRequest.StartPosition := planState.State.PlanningPosition;
            taskImplementationRequest.StartTime := planState.Time;
            for N in 1 .. Last (Get (State.m_projectedEntityStates, Unique_Request_Id)) loop
               pragma Loop_Invariant (Length (taskImplementationRequest.NeighborLocations) = To_Big_Integer (N -1));
               taskImplementationRequest.NeighborLocations := Add (taskImplementationRequest.NeighborLocations,
               Get (Get (State.m_projectedEntityStates, Unique_Request_Id), N).State);
            end loop;

            declare
               RemainingAssignments : TaskAssignment_Sequence := Element (State.m_remainingAssignments, Unique_Request_Id);
            begin
               RemainingAssignments := Remove (RemainingAssignments, TaskAssignment_Sequences.First);
               Replace (State.m_remainingAssignments, Unique_Request_Id, RemainingAssignments);
            end;

            sendBroadcastMessage (Mailbox, taskImplementationRequest);
         end;
      end if;
   end Send_Next_Task_Implementation_Request;

   procedure Process_Task_Implementation_Response
     (State            : in out Plan_Builder_State;
      Config           : in out Plan_Builder_Configuration_Data;
      Mailbox          : in out Plan_Builder_Mailbox;
      Received_Message : in out LMCP_Messages.TaskImplementationResponse)
   is
      Resp_ID : Int64 := Received_Message.ResponseID;
      Unique_Request_Id : Int64 := Element (State.m_expectedResponseID, Resp_ID);
      Vehicle_ID : Int64 := Received_Message.VehicleID;
      corrMish : MissionCommand;
      Position : Integer;
      Response_In_Progress : UniqueAutomationResponse := Element (State.m_inProgressResponse, Unique_Request_Id);
      MissionCo_List : MissionCommand_Seq := Response_In_Progress.MissionCommandList;
      WP : LMCP_Messages.Waypoint;
   begin

      if Length (Received_Message.TaskWaypoints) = 0 then
         declare
            Error_Msg : Unbounded_String;
         begin
            Error_Msg := Error_Msg & To_Unbounded_String ("Task [" & Received_Message.TaskID'Image & "]");
            Error_Msg := Error_Msg & To_Unbounded_String (" option [" & Received_Message.OptionID'Image & "]");
            Error_Msg := Error_Msg & To_Unbounded_String (" assigned to vehicle [" & Received_Message.VehicleID'Image & "]");
            Error_Msg := Error_Msg & To_Unbounded_String (" reported an empty waypoint list for implementation!");
            sendErrorMessage (Mailbox, Error_Msg);
               
            pragma Assert (Contains (State.m_inProgressResponse, Unique_Request_Id) and then Contains (State.m_reqeustIDVsOverrides, Unique_Request_Id));
            Check_Next_Task_Implementation_Request (Unique_Request_Id, Mailbox, State, Config);
            return;
         end;
      end if;

      declare
         Found : Boolean := False;
      begin
         for I in MC_Sequences.First .. Last (MissionCo_List) loop
            if Get (MissionCo_List, I).VehicleId = Vehicle_ID then
               pragma Assert (for some Mission_Co of MissionCo_List => Mission_Co.VehicleId = Vehicle_ID);
               corrMish := Get (MissionCo_List, I);
               Position := I;
               Found := True;

               if Length (corrMish.WaypointList) /= 0 then
                  WP :=  Get (corrMish.WaypointList, Last (corrMish.WaypointList));
                  WP.NextWaypoint := Common.Int64 (Get (Received_Message.TaskWaypoints, WP_Sequences.First).Number);
                  corrMish.WaypointList := Set (corrMish.WaypointList, Last (corrMish.WaypointList), WP);
               else
                  for W in 1 .. Last (Received_Message.TaskWaypoints) loop
                     pragma Loop_Invariant (Length (corrMish.WaypointList) = To_Big_Integer (W - 1));
                     corrMish.WaypointList := Add (corrMish.WaypointList, Get (Received_Message.TaskWaypoints, W));
                  end loop;
               end if;
               pragma Assert (Position in MC_Sequences.First .. Last (MissionCo_List));
               MissionCo_List := Set (MissionCo_List, Position, corrMish);
               Response_In_Progress.MissionCommandList := MissionCo_List;
               Replace (State.m_inProgressResponse, Unique_Request_Id, Response_In_Progress);
               exit;
            end if;
            pragma Loop_Invariant
               (for all K in 1 .. I => Get (MissionCo_List, K).VehicleId /= Vehicle_ID);
         end loop;

         if not Found then
            for Request_Id of State.m_reqeustIDVsOverrides loop
               declare
                  Overrides : SpeedAltPair_Sequence := Element (State.m_reqeustIDVsOverrides, Request_Id);
               begin
                  for I in SpeedAltPair_Sequences.First .. Last (Overrides) loop
                     declare
                        Speed_Alt_Pair : SpeedAltPair := Get (Overrides, I);
                     begin
                        if ((Speed_Alt_Pair.VehicleID = Vehicle_ID) and (Speed_Alt_Pair.TaskID = Received_Message.TaskID)) or (Speed_Alt_Pair.TaskID = 0) then
                           for I in WP_Sequences.First .. Last (Received_Message.TaskWaypoints) loop
                              pragma Loop_Invariant (Length (Received_Message.TaskWaypoints)'Loop_Entry = Length (Received_message.TaskWaypoints));
                              declare
                                 Way_Point : Waypoint := Get (Received_Message.TaskWaypoints, I);
                              begin
                                 Way_Point.Altitude := Speed_Alt_Pair.Altitude;
                                 Way_Point.Speed := Speed_Alt_Pair.Speed;
                                 Way_Point.AltitudeType := Speed_Alt_Pair.AltitudeType;
                                 for J in VA_Sequences.First .. Last (Way_Point.VehicleActionList) loop
                                    pragma Loop_Invariant (Length (Way_Point.VehicleActionList)'Loop_Entry = Length (Way_Point.VehicleActionList));
                                    declare
                                       Action : VehicleAction := Get (Way_Point.VehicleActionList, J);
                                    begin
                                       if Action.LoiterAction then
                                          Action.Location.Altitude := Speed_Alt_Pair.Altitude;
                                          Way_Point.VehicleActionList := Set (Way_Point.VehicleActionList, J, Action);
                                       end if;
                                    end;
                                 end loop;
                                 Received_Message.TaskWaypoints := Set (Received_Message.TaskWaypoints, I, Way_Point);
                              end;
                           end loop;
                        end if;
                     end;
                  end loop;
               end;
            end loop;
         end if;
      end;
   end Process_Task_Implementation_Response;

   procedure Check_Next_Task_Implementation_Request
     (Unique_Request_Id : Int64;
      Mailbox : in out Plan_Builder_Mailbox;
      State : in out Plan_Builder_State;
      Config : in out Plan_Builder_Configuration_Data)
   is
      Response : UniqueAutomationResponse := Element (State.m_inProgressResponse, Unique_Request_Id);
      Service_Status : ServiceStatus;
      Key_Value_Pair : KeyValuePair;
      In_Progress_Response : UniqueAutomationResponse := Element (State.m_inProgressResponse, Unique_Request_Id);
   begin
   
      if Contains (State.m_remainingAssignments, Unique_Request_Id) then
         if Length (Element (State.m_remainingAssignments, Unique_Request_Id)) = 0 then
            if Has_Key (State.m_projectedEntityStates, Unique_Request_Id) then
            pragma Assert (Length (In_Progress_Response.FinalStates) = 0);
               for E in 1 .. Last (Get (State.m_projectedEntityStates, Unique_Request_Id)) loop
                  pragma Loop_Invariant (Contains (State.m_inProgressResponse, Unique_Request_Id)
                     and Length (In_Progress_Response.FinalStates) = To_Big_Integer (E -1)
                     and E < Integer'Last);
                  In_Progress_Response.FinalStates := Add (In_Progress_Response.FinalStates, Get (Get (State.m_projectedEntityStates, Unique_Request_Id), E).State);
                  Replace (State.m_inProgressResponse, Unique_Request_Id, In_Progress_Response);
               end loop;
            end if;

            if Config.m_addLoiterToEndOfMission then
               pragma Assert (Length (Response.MissionCommandList) /= 0 and then Length (Get (Response.MissionCommandList, Last (Response.MissionCommandList)).WaypointList) /= 0);
               Add_Loiters_To_Mission_Commands (State, Config, Response);
            end if;

            if Config.m_overrideTurnType then
               for J in MC_Sequences.First .. Last (Response.MissionCommandList) loop
                  pragma Loop_Invariant (Length (Response.MissionCommandList)'Loop_Entry = Length (Response.MissionCommandList));
                  declare
                     Mission : MissionCommand := Get (Response.MissionCommandList, J);
                  begin
                     for I in Mission.WaypointList loop
                        declare
                           WP : LMCP_Messages.Waypoint := Get (Mission.WaypointList, I);
                        begin
                           WP.TurnType := Config.m_turnType;
                           Mission.WaypointList := Set (Mission.WaypointList, I, WP);
                        end;
                     end loop;
                     Response.MissionCommandList := Set (Response.MissionCommandList, J, Mission);
                  end;
               end loop;
            end if;

            sendBroadcastMessage (Mailbox, Response);

            Delete (State.m_inProgressResponse, Unique_Request_Id);
            Delete (State.m_reqeustIDVsOverrides, Unique_Request_Id);

            Service_Status.StatusType := Information;

            declare
               Message : Unbounded_String := To_Unbounded_String ("UniqueAutomationResponse[" & Unique_Request_Id'Image & "] - sent");
            begin
               Key_Value_Pair.Key := Message;
               Service_Status.Info := Add (Service_Status.Info, Key_Value_Pair);
               sendBroadcastMessage (Mailbox, Service_Status);
            end;
         else
            pragma Assert (
               Has_Key (State.m_projectedEntityStates, Unique_Request_Id)
               and then Contains (State.m_remainingAssignments, Unique_Request_Id)
               and then Length (Get (State.m_projectedEntityStates, Unique_Request_Id)) < To_Big_Integer (Integer'Last)
               and then Config.m_taskImplementationId < Int64'Last);
            Send_Next_Task_Implementation_Request (Unique_Request_Id, Mailbox, State, Config);
         end if;

      end if;

   end Check_Next_Task_Implementation_Request;

   -------------------------------------
   -- Add_Loiters_To_Mission_Commands --
   -------------------------------------                                                                                                                                                                                                                     

   procedure Add_Loiters_To_Mission_Commands
     (State : in Plan_Builder_State;
      Config : in Plan_Builder_Configuration_Data;
      Response : in out UniqueAutomationResponse)
   is
      Contains_Loiter : Boolean := False;
      Command : MissionCommand := Get (Response.MissionCommandList, MC_Sequences.First);
      Target_Vehicle : Int64 := Command.VehicleId;
   begin

      for Mission of Response.MissionCommandList loop

         for Way_Point of Mission.WaypointList loop
            for Action of Way_Point.VehicleActionList loop
               if Action.LoiterAction then
                  Contains_Loiter := True;
               end if;
            end loop;
         end loop;
      end loop;

      if Contains_Loiter then
         return;
      end if;

      for Mission of Response.MissionCommandList loop
         if Mission.VehicleId /= Target_Vehicle then
            --      IMPACT_INFORM("Automation Response contains mission commands for multiple vehicles. No loiters!!");
            return;
         end if;
      end loop;

      if Has_Key (State.m_currentEntityStates, Target_Vehicle) then
         declare
            Entity_State : EntityState := ES_Maps.Get (State.m_currentEntityStates, Target_Vehicle);
            Last_Mission_Command : MissionCommand := Get (Response.MissionCommandList, Last (Response.MissionCommandList));
            Wp_List : WP_Seq := Last_Mission_Command.WaypointList;
            Back_El : LMCP_Messages.Waypoint := Get (Wp_List, Last (Wp_List));
            Loiter_Action : VehicleAction;
         begin
            if Entity_State.isGroundVehicleState or Entity_State.isSurfaceVehicleState then
               Loiter_Action.LoiterType :=  Hover;
            else
               Loiter_Action.LoiterType :=  Circular;
               Loiter_Action.Radius := Config.m_deafultLoiterRadius;
            end if;
            Loiter_Action.Location := (Back_El.Latitude, Back_El.Longitude, Back_El.Altitude, Back_El.AltitudeType);
            --  Loiter_Action.Airspeed := Back_El.GroundSpeed;
            Back_El.VehicleActionList := Add (Back_El.VehicleActionList, Loiter_Action);
            Wp_List := Set (Wp_List, Last(Wp_List), Back_El);
            Last_Mission_Command.WaypointList := Wp_List;
            Response.MissionCommandList := Set (Response.MissionCommandList, Last (Response.MissionCommandList), Last_Mission_Command);
         end;
      end if;
   end Add_Loiters_To_Mission_Commands;

end Plan_Builder;
