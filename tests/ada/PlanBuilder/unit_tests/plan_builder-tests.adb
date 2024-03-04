--
--  Copyright (C) 2024, AdaCore
--

with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Common; use Common;

with Plan_builder; use Plan_builder;
with Plan_Builder_Communication; use Plan_Builder_Communication;
with LMCP_Messages; use LMCP_Messages;

package body Plan_Builder.Tests is

   procedure Test_Process_Task_Assignment_Summary (T : in out Test) is
      pragma Unreferenced (T);
      use all type Int64_Seq;
      use Int64_UAReq_Maps;
      use Int64_PS_Maps;
      use Int64_UAResp_Maps;
      Message : LMCP_Messages.TaskAssignmentSummary;
      State : Plan_Builder_State;
      Config : Plan_Builder_Configuration_Data;
      Mailbox : Plan_Builder_Mailbox;
      Automation_Request : UniqueAutomationRequest;
   begin      Plan_Builder_Communication.Initialize(Mailbox, "TestMailbox", 1, 1, 1);
      -- Corresponding Automation Request ID exists
      Message.CorrespondingAutomationRequestID := 1;

      declare
         TaskAssignment_1 : TaskAssignment := (10, 0, 0, 0, 0);
         TaskAssignment_2 : TaskAssignment := (11, 0, 0, 0, 0);
      begin
         Message.TaskList := Add (Message.TaskList, TaskAssignment_1);
         Message.TaskList := Add (Message.TaskList, TaskAssignment_2);
      end;

      declare
         Entity_State_1 : EntityState := (Id => 100, Location => (1.0, 1.0, 1.0, MSL), Heading => 0.0, Time => 350, GroundSpeed => 54.5, isGroundVehicleState => False, isAirVehicleState => False, isStationarySensorState => False, isSurfaceVehicleState => False);
         Entity_State_2 : EntityState := (Id => 101, Location => (1.0, 1.0, 1.0, MSL), Heading => 0.0, Time => 350, GroundSpeed => 54.5, isGroundVehicleState => False, isAirVehicleState => False, isStationarySensorState => False, isSurfaceVehicleState => False);
      begin
         State.m_currentEntityStates := ES_Maps.Add (State.m_currentEntityStates, Common.Int64 (100), Entity_State_1);
         State.m_currentEntityStates := ES_Maps.Add (State.m_currentEntityStates, Common.Int64 (101), Entity_State_2);
      end;

      Automation_Request.EntityList := Add (Automation_Request.EntityList, Common.Int64 (100));
      Automation_Request.EntityList := Add (Automation_Request.EntityList, Common.Int64 (101));

      Declare
         Planning_State_1 : PlanningState := (EntityID => 100, PlanningPosition => (1.0, 1.0, 1.0, MSL), PlanningHeading => 0.0);
         Planning_State_2 : PlanningState := (EntityID => 100, PlanningPosition => (2.0, 2.0, 2.0, AGL), PlanningHeading => 1.0);
         Planning_State_3 : PlanningState := (EntityID => 101, PlanningPosition => (3.0, 3.0, 3.0, MSL), PlanningHeading => 0.0);
         Planning_State_4 : PlanningState := (EntityID => 102, PlanningPosition => (4.0, 4.0, 4.0, AGL), PlanningHeading => 1.0);
      begin
         Automation_Request.PlanningStates := Add (Automation_Request.PlanningStates, Planning_State_1);
         Automation_Request.PlanningStates := Add (Automation_Request.PlanningStates, Planning_State_2);
         Automation_Request.PlanningStates := Add (Automation_Request.PlanningStates, Planning_State_3);
         Automation_Request.PlanningStates := Add (Automation_Request.PlanningStates, Planning_State_4);
      end;

      Insert (State.m_uniqueAutomationRequests, Common.Int64 (1), Automation_Request);

      Process_Task_Assignment_Summary (State, Config, Mailbox, Message);

      Assert (Contains (State.m_assignmentSummaries, Message.CorrespondingAutomationRequestID), "Test Failed : assignment suummaries not updated.");
      Assert (Contains (State.m_inProgressResponse, Message.CorrespondingAutomationRequestID), "Test Failed : In progress response missing.");
      Assert (Has_Key (State.m_projectedEntityStates, Message.CorrespondingAutomationRequestID), "Test Failed : Missing Project Entity States");
   end Test_Process_Task_Assignment_Summary;

   procedure Test_Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test (T : in out Test) is
      pragma Unreferenced (T);
      use all type Int64_Seq;
      use Int64_UAResp_Maps;
      use Int64_ER_Maps;
      Received_Message : LMCP_Messages.TaskImplementationResponse;
      State : Plan_Builder_State;
      Config : Plan_Builder_Configuration_Data;
      Mailbox : Plan_Builder_Mailbox;
      Automation_Request : UniqueAutomationRequest;
      Response_In_Progress : UniqueAutomationResponse;
   begin
      Plan_Builder_Communication.Initialize(Mailbox, "TestMailbox", 1, 1, 1);
      Received_Message.ResponseID := Common.Int64 (1);
      Received_Message.VehicleID := Common.Int64 (100);

      Response_In_Progress.ResponseID := Common.Int64 (1);
 
      Insert (State.m_expectedResponseID,  Common.Int64 (1),  Common.Int64 (1));

      declare
         WayPoint_1 : WayPoint;
         WayPoint_2 : WayPoint;
      begin
         WayPoint_1.Number := 1;
         WayPoint_2.Number := 2;
         Received_Message.TaskWaypoints := Add (Received_Message.TaskWaypoints, WayPoint_1);
         Received_Message.TaskWaypoints := Add (Received_Message.TaskWaypoints, WayPoint_2);
      end;

      declare
         Mission_Command_1 : MissionCommand;
         Mission_Command_2 : MissionCommand;
      begin
         Mission_Command_1.VehicleId := 100;
         Mission_Command_2.VehicleId := 101;
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command_1);
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command_2);
         Assert (Length (Mission_Command_1.WaypointList) = 0, "Test Failed : Mission Commands Initilization failed.");
      end;

      Insert (State.m_inProgressResponse, Common.Int64 (1), Response_In_Progress);

      Process_Task_Implementation_Response (State, Config, Mailbox, Received_Message);

      declare
         Response : UniqueAutomationResponse := Element (State.m_inProgressResponse, Common.Int64 (1));
         MissionCo_List : MissionCommand_Seq := Response.MissionCommandList;
         MC : MissionCommand;
      begin

         for I in MC_Sequences.First .. Last (MissionCo_List) loop
            if Get (MissionCo_List, I).VehicleId = Received_Message.VehicleID then
               MC := Get (MissionCo_List, I);
            end if;
         end loop;
         Assert (Length (MC.WaypointList) /= 0, "Test Failed : Waypoints not added to Mission Commands.");
      end;
   end Test_Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test;

   procedure Test_Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty (T : in out Test) is
      pragma Unreferenced (T);
      use all type Int64_Seq;
      use Int64_UAResp_Maps;
      use Int64_ER_Maps;
      Received_Message : LMCP_Messages.TaskImplementationResponse;
      State : Plan_Builder_State;
      Config : Plan_Builder_Configuration_Data;
      Mailbox : Plan_Builder_Mailbox;
      Automation_Request : UniqueAutomationRequest;
      Response_In_Progress : UniqueAutomationResponse;
   begin
      Plan_Builder_Communication.Initialize(Mailbox, "TestMailbox", 1, 1, 1);
      Received_Message.ResponseID := Common.Int64 (1);
      Received_Message.VehicleID := Common.Int64 (100);
      Response_In_Progress.ResponseID := Common.Int64 (1);
      Insert (State.m_expectedResponseID,  Common.Int64 (1),  Common.Int64 (1));
      declare
         WayPoint_1 : WayPoint;
         WayPoint_2 : WayPoint;
      begin
         WayPoint_1.Number := 1;
         WayPoint_2.Number := 2;
         Received_Message.TaskWaypoints := Add (Received_Message.TaskWaypoints, WayPoint_1);
         Received_Message.TaskWaypoints := Add (Received_Message.TaskWaypoints, WayPoint_2);
      end;

      declare
         Mission_Command_1 : MissionCommand;
         Mission_Command_2 : MissionCommand;
         WayPoint_1 : WayPoint;
         WayPoint_2 : WayPoint;
      begin
         Mission_Command_1.VehicleId := 100;
         Mission_Command_2.VehicleId := 101;
         WayPoint_1.Number := 1;
         WayPoint_2.Number := 2;
         Mission_Command_1.WaypointList := Add (Mission_Command_1.WaypointList, WayPoint_1);
         Mission_Command_1.WaypointList := Add (Mission_Command_1.WaypointList, WayPoint_2);
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command_1);
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command_2);
         Assert (Get (Mission_Command_1.WaypointList, Last (Mission_Command_1.WaypointList)).NextWaypoint /= Get (Received_Message.TaskWaypoints, WP_Sequences.First).Number, "Test Failed : Mission Commands initilization not valid.");
      end;

      Insert (State.m_inProgressResponse, Common.Int64 (1), Response_In_Progress);

      Process_Task_Implementation_Response (State, Config, Mailbox, Received_Message);
   
      declare
         Response : UniqueAutomationResponse := Element (State.m_inProgressResponse, Common.Int64 (1));
         MissionCo_List : MissionCommand_Seq := Response.MissionCommandList;
         MC : MissionCommand;
      begin

         for I in MC_Sequences.First .. Last (MissionCo_List) loop
            if Get (MissionCo_List, I).VehicleId = Received_Message.VehicleID then
               MC := Get (MissionCo_List, I);
            end if;
         end loop;
         Assert (Get (MC.WaypointList, Last (MC.WaypointList)).NextWaypoint = Get (Received_Message.TaskWaypoints, WP_Sequences.First).Number, "Test Failed : Waypoints not added to Mission Commands.");
      end;
   end Test_Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty;

   procedure Test_Process_Task_Implementation_Response_Vehicle_DoesNotExists (T : in out Test) is
      pragma Unreferenced (T);
      use all type Int64_Seq;
      use Int64_UAResp_Maps;
      use Int64_ER_Maps;
      Received_Message : LMCP_Messages.TaskImplementationResponse;
      State : Plan_Builder_State;
      Config : Plan_Builder_Configuration_Data;
      Mailbox : Plan_Builder_Mailbox;
      Automation_Request : UniqueAutomationRequest;
      Response_In_Progress : UniqueAutomationResponse;
   begin
      Plan_Builder_Communication.Initialize(Mailbox, "TestMailbox", 1, 1, 1);
      Received_Message.ResponseID := Common.Int64 (1);
      Received_Message.VehicleID := Common.Int64 (102);
      Response_In_Progress.ResponseID := Common.Int64 (1);
      Insert (State.m_expectedResponseID,  Common.Int64 (1),  Common.Int64 (1));
      declare
         WayPoint_1 : WayPoint;
         WayPoint_2 : WayPoint;
      begin
         WayPoint_1.Number := 1;
         WayPoint_2.Number := 2;
         Received_Message.TaskWaypoints := Add (Received_Message.TaskWaypoints, WayPoint_1);
         Received_Message.TaskWaypoints := Add (Received_Message.TaskWaypoints, WayPoint_2);
      end;

      declare
         Mission_Command_1 : MissionCommand;
         Mission_Command_2 : MissionCommand;
         WayPoint_1 : WayPoint;
         WayPoint_2 : WayPoint;
      begin
         Mission_Command_1.VehicleId := 100;
         Mission_Command_2.VehicleId := 101;
         WayPoint_1.Number := 1;
         WayPoint_2.Number := 2;
         Mission_Command_1.WaypointList := Add (Mission_Command_1.WaypointList, WayPoint_1);
         Mission_Command_1.WaypointList := Add (Mission_Command_1.WaypointList, WayPoint_2);
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command_1);
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command_2);
      end;

      Insert (State.m_inProgressResponse, Common.Int64 (1), Response_In_Progress);

      Process_Task_Implementation_Response (State, Config, Mailbox, Received_Message);

      declare
         MC_List : MissionCommand_Seq := Element (State.m_inProgressResponse, Common.Int64 (1)).MissionCommandList;
      begin
         Assert (Get (MC_List, Last(MC_List)).FirstWaypoint = Get (Received_Message.TaskWaypoints, WP_Sequences.First).Number, "Incorrect result");
         Assert (Get (MC_List, Last(MC_List)).VehicleId = Received_Message.VehicleID, "Incorrect result");
      end;
   end Test_Process_Task_Implementation_Response_Vehicle_DoesNotExists;

   procedure Test_Send_Next_Task_Implementation_Request (T : in out Test) is
      pragma Unreferenced (T);
      use Int64_UAReq_Maps;
      use Int64_TAL_Maps;
      use Int64_PS_Maps;
      Req_ID : Int64 := Common.Int64 (1);
      Automation_Request : UniqueAutomationRequest;
      State : Plan_Builder_State;
      Config : Plan_Builder_Configuration_Data;
      Mailbox : Plan_Builder_Mailbox;
   begin
      Plan_Builder_Communication.Initialize(Mailbox, "TestMailbox", 1, 1, 1);
      Automation_Request.RequestID := Req_ID;
      Insert (State.m_uniqueAutomationRequests, Common.Int64 (1), Automation_Request);
      declare
         TaskAssignment_1 : TaskAssignment := (10, 0, 0, 0, 0);
         TaskAssignment_2 : TaskAssignment := (11, 0, 0, 0, 0);
         RemainingAssignments : TaskAssignment_Sequence;
      begin
         RemainingAssignments := Add (RemainingAssignments, TaskAssignment_1);
         RemainingAssignments := Add (RemainingAssignments, TaskAssignment_2);
         Insert (State.m_remainingAssignments, Req_ID, RemainingAssignments);
      end;

      declare
         Projected_State_1 : ProjectedState := ProjectedState'(State => PlanningState'(EntityID => 1, PlanningPosition => LMCP_Messages.Location3D'(Latitude => 0.0, Longitude => 0.0, Altitude => 0.0, AltitudeType => MSL), PlanningHeading => 0.0), FinalWaypointID => 0, Time => 0);
         Projected_State_2 : ProjectedState;
         Projected_States : ProjectedState_Seq;
      begin
         Projected_States := Add (Projected_States, Projected_State_1);
         Projected_States := Add (Projected_States, Projected_State_2);
         State.m_projectedEntityStates := Add (State.m_projectedEntityStates, Req_ID, Projected_States);
      end;

      Send_Next_Task_Implementation_Request (Req_ID, Mailbox, State, Config);

      Assert (Length ( Element (State.m_remainingAssignments, Req_ID)) = 1, "Incorrect result");
   end Test_Send_Next_Task_Implementation_Request;

   procedure Test_Check_Next_Task_Implementation_Request (T : in out Test) is
      pragma Unreferenced (T);
      use Int64_TAL_Maps;
      use Int64_PS_Maps;
      use Int64_UAResp_Maps;
      use Int64_ROR_Maps;
      Unique_Request_Id : Int64 := 1;
      Mailbox : Plan_Builder_Mailbox;
      State : Plan_Builder_State;
      Config : Plan_Builder_Configuration_Data;
      Tasks : TaskAssignment_Sequence;
      Response_In_Progress : UniqueAutomationResponse;
   begin
      Plan_Builder_Communication.Initialize(Mailbox, "TestMailbox", 1, 1, 1);
      Insert (State.m_remainingAssignments, Unique_Request_Id, Tasks);
      declare
         Projected_State_1 : ProjectedState := ProjectedState'(State => PlanningState'(EntityID => 1, PlanningPosition => LMCP_Messages.Location3D'(Latitude => 0.0, Longitude => 0.0, Altitude => 0.0, AltitudeType => MSL), PlanningHeading => 0.0), FinalWaypointID => 0, Time => 0);
         Projected_State_2 : ProjectedState;
         Projected_States : ProjectedState_Seq;
      begin
         Projected_States := Add (Projected_States, Projected_State_1);
         Projected_States := Add (Projected_States, Projected_State_2);
         State.m_projectedEntityStates := Add (State.m_projectedEntityStates, Unique_Request_Id, Projected_States);
      end;

      declare
         Mission_Command_1 : MissionCommand;
         Mission_Command_2 : MissionCommand;
         WayPoint_1 : WayPoint;
         WayPoint_2 : WayPoint;
      begin
         Mission_Command_1.VehicleId := 100;
         Mission_Command_2.VehicleId := 101;
         WayPoint_1.Number := 1;
         WayPoint_2.Number := 2;
         Mission_Command_1.WaypointList := Add (Mission_Command_1.WaypointList, WayPoint_1);
         Mission_Command_1.WaypointList := Add (Mission_Command_1.WaypointList, WayPoint_2);
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command_1);
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command_2);
      end;

      Insert (State.m_inProgressResponse, Common.Int64 (1), Response_In_Progress);

      declare
         SpeedAltPair_1 : SpeedAltPair := (VehicleID =>  1, TaskID =>  0, Speed =>  0.00000E+00, Altitude =>  0.0, AltitudeType => AGL);
         SpeedAltPair_2 : SpeedAltPair := (VehicleID =>  2, TaskID =>  0, Speed =>  0.00000E+00, Altitude =>  0.0, AltitudeType => AGL);
         Seq : SpeedAltPair_Sequence;
      begin
         Seq := Add (Seq, SpeedAltPair_1);
         Seq := Add (Seq, SpeedAltPair_2);
         Insert (State.m_reqeustIDVsOverrides, Common.Int64 (1), Seq);
      end;

      declare
         SpeedAltPair_1 : SpeedAltPair := (VehicleID =>  3, TaskID =>  0, Speed =>  0.00000E+00, Altitude =>  0.0, AltitudeType => AGL);
         SpeedAltPair_2 : SpeedAltPair := (VehicleID =>  4, TaskID =>  0, Speed =>  0.00000E+00, Altitude =>  0.0, AltitudeType => AGL);
         Seq : SpeedAltPair_Sequence;
      begin
         Seq := Add (Seq, SpeedAltPair_1);
         Seq := Add (Seq, SpeedAltPair_2);
         Insert (State.m_reqeustIDVsOverrides, Common.Int64 (2), Seq);
      end;

      Config.m_addLoiterToEndOfMission := True;
      Config.m_overrideTurnType := True;

      Check_Next_Task_Implementation_Request(Unique_Request_Id, Mailbox, State, Config);
      Assert (not Contains (State.m_inProgressResponse, Unique_Request_Id), "Test Failed : In progress response has not been deleted.");
      Assert (not Contains (State.m_reqeustIDVsOverrides, Unique_Request_Id), "Test Failed : Request overrides have not been deleted.");
   end Test_Check_Next_Task_Implementation_Request;

   procedure Test_Add_Loiters_To_Mission_Commands_Does_Not_Contain_Loiters_Hover (T : in out Test) is
      pragma Unreferenced (T);
      use Int64_UAResp_Maps;
      Unique_Request_Id : Int64 := 1;
      Response_In_Progress : UniqueAutomationResponse;
      State : Plan_Builder_State;
      Config : Plan_Builder_Configuration_Data;
   begin
      declare
         Entity_State_1 : EntityState := (Id => 100, Location => (1.0, 1.0, 1.0, MSL), Heading => 0.0, Time => 350, GroundSpeed => 54.5, isGroundVehicleState => True, isAirVehicleState => False, isStationarySensorState => False, isSurfaceVehicleState => False);
         Entity_State_2 : EntityState := (Id => 101, Location => (1.0, 1.0, 1.0, MSL), Heading => 0.0, Time => 350, GroundSpeed => 54.5, isGroundVehicleState => False, isAirVehicleState => False, isStationarySensorState => False, isSurfaceVehicleState => False);
      begin
         State.m_currentEntityStates := ES_Maps.Add (State.m_currentEntityStates, Common.Int64 (100), Entity_State_1);
         State.m_currentEntityStates := ES_Maps.Add (State.m_currentEntityStates, Common.Int64 (101), Entity_State_2);
      end;
      declare
         Mission_Command : MissionCommand;
         WayPoint_1 : WayPoint;
         WayPoint_2 : WayPoint;
         Action_1 : VehicleAction;
         Action_2 : VehicleAction;
      begin
         Config.m_deafultLoiterRadius := 10.0;
         Mission_Command.VehicleId := 100;
         Action_1.LoiterAction := False;
         WayPoint_1.Number := 1;
         WayPoint_2.Number := 2;
         WayPoint_2.VehicleActionList := Add (WayPoint_1.VehicleActionList, Action_1);
         WayPoint_2.VehicleActionList := Add (WayPoint_1.VehicleActionList, Action_2);
         Mission_Command.WaypointList := Add (Mission_Command.WaypointList, WayPoint_1);
         Mission_Command.WaypointList := Add (Mission_Command.WaypointList, WayPoint_2);
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command);
      end;

      Insert (State.m_inProgressResponse, Unique_Request_Id, Response_In_Progress);

      Add_Loiters_To_Mission_Commands (State, Config, Unique_Request_Id);
      declare
         Wp_List : WP_Seq := Get (Element (State.m_inProgressResponse, Unique_Request_Id).MissionCommandList, Last (Element (State.m_inProgressResponse, Unique_Request_Id).MissionCommandList)).WaypointList;
         Last_WP : LMCP_Messages.Waypoint := Get (Wp_List, Last (Wp_List));
         VA : VehicleAction := Get (Last_WP.VehicleActionList, Last (Last_WP.VehicleActionList));
      begin
         Assert (VA.Radius /= Config.m_deafultLoiterRadius, "Incorrect result");
         Assert (VA.LoiterType = Hover, "Incorrect result" & LoiterTypeEnum'Image(VA.LoiterType));
      end;
   end Test_Add_Loiters_To_Mission_Commands_Does_Not_Contain_Loiters_Hover;

   procedure Test_Add_Loiters_To_Mission_Commands_Does_Not_Contain_Loiters_Circular (T : in out Test) is
      pragma Unreferenced (T);
      use Int64_UAResp_Maps;
      Unique_Request_Id : Int64 := 1;
      Response_In_Progress : UniqueAutomationResponse;
      State : Plan_Builder_State;
      Config : Plan_Builder_Configuration_Data;
   begin
      declare
         Entity_State_1 : EntityState := (Id => 100, Location => (1.0, 1.0, 1.0, MSL), Heading => 0.0, Time => 350, GroundSpeed => 54.5, isGroundVehicleState => False, isAirVehicleState => False, isStationarySensorState => False, isSurfaceVehicleState => False);
         Entity_State_2 : EntityState := (Id => 101, Location => (1.0, 1.0, 1.0, MSL), Heading => 0.0, Time => 350, GroundSpeed => 54.5, isGroundVehicleState => False, isAirVehicleState => False, isStationarySensorState => False, isSurfaceVehicleState => False);
      begin
         State.m_currentEntityStates := ES_Maps.Add (State.m_currentEntityStates, Common.Int64 (100), Entity_State_1);
         State.m_currentEntityStates := ES_Maps.Add (State.m_currentEntityStates, Common.Int64 (101), Entity_State_2);
      end;
      declare
         Mission_Command : MissionCommand;
         WayPoint_1 : WayPoint;
         WayPoint_2 : WayPoint;
         Action_1 : VehicleAction;
         Action_2 : VehicleAction;
      begin
         Config.m_deafultLoiterRadius := 10.0;
         Mission_Command.VehicleId := 100;
         Action_1.LoiterAction := False;
         WayPoint_1.Number := 1;
         WayPoint_2.Number := 2;
         WayPoint_2.VehicleActionList := Add (WayPoint_1.VehicleActionList, Action_1);
         WayPoint_2.VehicleActionList := Add (WayPoint_1.VehicleActionList, Action_2);
         Mission_Command.WaypointList := Add (Mission_Command.WaypointList, WayPoint_1);
         Mission_Command.WaypointList := Add (Mission_Command.WaypointList, WayPoint_2);
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command);
      end;

      Insert (State.m_inProgressResponse, Unique_Request_Id, Response_In_Progress);

      Add_Loiters_To_Mission_Commands (State, Config, Unique_Request_Id);
      declare
         Wp_List : WP_Seq := Get (Element (State.m_inProgressResponse, Unique_Request_Id).MissionCommandList, Last (Element (State.m_inProgressResponse, Unique_Request_Id).MissionCommandList)).WaypointList;
         Last_WP : LMCP_Messages.Waypoint := Get (Wp_List, Last (Wp_List));
         VA : VehicleAction := Get (Last_WP.VehicleActionList, Last (Last_WP.VehicleActionList));
      begin
         Assert (VA.Radius = Config.m_deafultLoiterRadius, "Incorrect result" & VA.Radius'Image);
         Assert (VA.LoiterType = Circular, "Incorrect result" & LoiterTypeEnum'Image(VA.LoiterType));
      end;
   end Test_Add_Loiters_To_Mission_Commands_Does_Not_Contain_Loiters_Circular;

   procedure Test_Add_Loiters_To_Mission_Commands_Contains_Loiters (T : in out Test) is
      pragma Unreferenced (T);
      Unique_Request_Id : Int64 := 1;
      Response_In_Progress : UniqueAutomationResponse;
      State : Plan_Builder_State;
      Config : Plan_Builder_Configuration_Data;
   begin
      declare
         Entity_State_1 : EntityState := (Id => 100, Location => (1.0, 1.0, 1.0, MSL), Heading => 0.0, Time => 350, GroundSpeed => 54.5, isGroundVehicleState => False, isAirVehicleState => False, isStationarySensorState => False, isSurfaceVehicleState => False);
         Entity_State_2 : EntityState := (Id => 101, Location => (1.0, 1.0, 1.0, MSL), Heading => 0.0, Time => 350, GroundSpeed => 54.5, isGroundVehicleState => False, isAirVehicleState => False, isStationarySensorState => False, isSurfaceVehicleState => False);
      begin
         State.m_currentEntityStates := ES_Maps.Add (State.m_currentEntityStates, Common.Int64 (100), Entity_State_1);
         State.m_currentEntityStates := ES_Maps.Add (State.m_currentEntityStates, Common.Int64 (101), Entity_State_2);
      end;
      declare
         Mission_Command : MissionCommand;
         WayPoint_1 : WayPoint;
         WayPoint_2 : WayPoint;
         Action_1 : VehicleAction;
         Action_2 : VehicleAction;
      begin
         Config.m_deafultLoiterRadius := 10.0;
         Mission_Command.VehicleId := 100;
         Action_1.LoiterAction := False;
         Action_2.LoiterAction := False;
         Action_2.Radius := 0.0;
         WayPoint_1.Number := 1;
         WayPoint_2.Number := 2;
         WayPoint_2.VehicleActionList := Add (WayPoint_1.VehicleActionList, Action_1);
         WayPoint_2.VehicleActionList := Add (WayPoint_1.VehicleActionList, Action_2);
         Mission_Command.WaypointList := Add (Mission_Command.WaypointList, WayPoint_1);
         Mission_Command.WaypointList := Add (Mission_Command.WaypointList, WayPoint_2);
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command);
      end;

      Insert (State.m_inProgressResponse, Unique_Request_Id, Response_In_Progress);

      Add_Loiters_To_Mission_Commands (State, Config, Unique_Request_Id);
      declare
         Wp_List : WP_Seq := Get (Element (State.m_inProgressResponse, Unique_Request_Id).MissionCommandList, Last (Element (State.m_inProgressResponse, Unique_Request_Id).MissionCommandList)).WaypointList;
         Last_WP : LMCP_Messages.Waypoint := Get (Wp_List, Last (Wp_List));
         VA : VehicleAction := Get (Last_WP.VehicleActionList, Last (Last_WP.VehicleActionList));
      begin
         Assert (Element (State.m_inProgressResponse, Unique_Request_Id) = Response_In_Progress, "Incorrect result");
      end;
   end Test_Add_Loiters_To_Mission_Commands_Contains_Loiters;

   procedure Test_Add_Loiters_To_Mission_Commands_Multiple_Vehicles (T : in out Test) is
      pragma Unreferenced (T);
      use Int64_UAResp_Maps;
      Response_In_Progress : UniqueAutomationResponse;
      Unique_Request_Id : Int64 := 1;
      State : Plan_Builder_State;
      Config : Plan_Builder_Configuration_Data;
   begin
      declare
         Entity_State_1 : EntityState := (Id => 100, Location => (1.0, 1.0, 1.0, MSL), Heading => 0.0, Time => 350, GroundSpeed => 54.5, isGroundVehicleState => False, isAirVehicleState => False, isStationarySensorState => False, isSurfaceVehicleState => False);
         Entity_State_2 : EntityState := (Id => 101, Location => (1.0, 1.0, 1.0, MSL), Heading => 0.0, Time => 350, GroundSpeed => 54.5, isGroundVehicleState => False, isAirVehicleState => False, isStationarySensorState => False, isSurfaceVehicleState => False);
      begin
         State.m_currentEntityStates := ES_Maps.Add (State.m_currentEntityStates, Common.Int64 (100), Entity_State_1);
         State.m_currentEntityStates := ES_Maps.Add (State.m_currentEntityStates, Common.Int64 (101), Entity_State_2);
      end;
      declare
         Mission_Command_1 : MissionCommand;
         Mission_Command_2 : MissionCommand;
         WayPoint_1 : WayPoint;
         WayPoint_2 : WayPoint;
         Action_1 : VehicleAction;
         Action_2 : VehicleAction;
      begin
         Config.m_deafultLoiterRadius := 10.0;
         Mission_Command_1.VehicleId := 100;
         Mission_Command_2.VehicleId := 101;
         Action_2.LoiterAction := True;
         Action_2.Radius := 0.0;
         WayPoint_1.Number := 1;
         WayPoint_2.Number := 2;
         WayPoint_2.VehicleActionList := Add (WayPoint_1.VehicleActionList, Action_1);
         WayPoint_2.VehicleActionList := Add (WayPoint_1.VehicleActionList, Action_2);
         Mission_Command_1.WaypointList := Add (Mission_Command_1.WaypointList, WayPoint_1);
         Mission_Command_1.WaypointList := Add (Mission_Command_1.WaypointList, WayPoint_2);
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command_2);
         Response_In_Progress.MissionCommandList := Add (Response_In_Progress.MissionCommandList, Mission_Command_1);
      end;

      Insert (State.m_inProgressResponse, Unique_Request_Id, Response_In_Progress);

      Add_Loiters_To_Mission_Commands (State, Config, Unique_Request_Id);
      declare
         Wp_List : WP_Seq := Get (Element (State.m_inProgressResponse, Unique_Request_Id).MissionCommandList, Last (Element (State.m_inProgressResponse, Unique_Request_Id).MissionCommandList)).WaypointList;
         Last_WP : LMCP_Messages.Waypoint := Get (Wp_List, Last (Wp_List));
         VA : VehicleAction := Get (Last_WP.VehicleActionList, Last (Last_WP.VehicleActionList));
      begin
         Assert (Element (State.m_inProgressResponse, Unique_Request_Id) = Response_In_Progress, "Incorrect result");
      end;
   end Test_Add_Loiters_To_Mission_Commands_Multiple_Vehicles;
end Plan_Builder.Tests;