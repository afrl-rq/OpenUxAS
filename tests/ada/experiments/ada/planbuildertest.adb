with Ada.Text_IO; use Ada.Text_IO;
with Plan_Builder; use Plan_Builder;
with Plan_Builder_Communication; use Plan_Builder_Communication;
with Common; use Common;
with LMCP_Messages; use LMCP_Messages;
with Ada.Assertions; use Ada.Assertions;
with State_Serializer; use State_Serializer;

package body PlanBuilderTest is

   procedure Process_Task_Assignment_Summary_Test is
      use all type Int64_Seq;
      use Int64_UAReq_Maps;
      Message : LMCP_Messages.TaskAssignmentSummary;
      State : Plan_Builder_State;
      Config : Plan_Builder_Configuration_Data;
      Mailbox : Plan_Builder_Mailbox;
      Automation_Request : UniqueAutomationRequest;
   begin
      Plan_Builder_Communication.Initialize(Mailbox, "TestGroup", 1, 1, 1);
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

      -- planning states
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

      Automation_Request.RequestID := 1;

      Insert (State.m_uniqueAutomationRequests, Common.Int64 (1), Automation_Request);

      Process_Task_Assignment_Summary (State, Config, Mailbox, Message);

      Write_State_To_File (State, "Ada_Process_Task_Assignment_Summary_Test.txt");
   end Process_Task_Assignment_Summary_Test;

   procedure Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test is
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
      Plan_Builder_Communication.Initialize(Mailbox, "TestGroup", 1, 1, 1);
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
      end;

      Insert (State.m_inProgressResponse, Common.Int64 (1), Response_In_Progress);

      Process_Task_Implementation_Response (State, Config, Mailbox, Received_Message);

      Write_State_To_File (State, "Ada_Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test.txt");
   end Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test;

   procedure Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty_Test is
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
      Plan_Builder_Communication.Initialize(Mailbox, "TestGroup", 1, 1, 1);
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
      end;

      Insert (State.m_inProgressResponse, Common.Int64 (1), Response_In_Progress);

      Process_Task_Implementation_Response (State, Config, Mailbox, Received_Message);

      Write_State_To_File (State, "Ada_Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty_Test.txt");
   end Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty_Test;

   procedure Process_Task_Implementation_Response_Vehicle_DoesNotExists_Test is
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
      Plan_Builder_Communication.Initialize(Mailbox, "TestGroup", 1, 1, 1);
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

      Write_State_To_File (State, "Ada_Process_Task_Implementation_Response_Vehicle_DoesNotExists_Test.txt");

   end Process_Task_Implementation_Response_Vehicle_DoesNotExists_Test;

   procedure Check_Next_Task_Implementation_Request_Test is
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
      Plan_Builder_Communication.Initialize(Mailbox, "TestGroup", 1, 1, 1);
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

      Check_Next_Task_Implementation_Request(Unique_Request_Id, Mailbox, State, Config);

      Write_State_To_File (State, "Ada_Check_Next_Task_Implementation_Request_Test.txt");
   end Check_Next_Task_Implementation_Request_Test;

end PlanBuilderTest;
