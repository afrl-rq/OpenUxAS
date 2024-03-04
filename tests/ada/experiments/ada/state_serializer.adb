with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with LMCP_Messages; use LMCP_Messages;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Common; use Common;
with Plan_Builder; use Plan_Builder;

package body State_Serializer is

   -- Helper procedure to write a string to the file
   procedure Write_String_To_File
     (File      : in File_Type;
      Text      : in Unbounded_String) is
   begin
      Put_Line (File, Text);
   end Write_String_To_File;

   -- Helper function to convert PlanningState to string
   function PlanningState_To_String
     (Planning_State : PlanningState) return Unbounded_String is
   begin
      return To_Unbounded_String("PlanningState: EntityID => " & Int64'Image (Planning_State.EntityID) &
             ", Latitude => " & Real64'Image (Planning_State.PlanningPosition.Latitude) &
             ", Longitude => " & Real64'Image (Planning_State.PlanningPosition.Longitude) &
             ", Altitude => " & Real64'Image (Real64 (Planning_State.PlanningPosition.Altitude)) &
             ", AltitudeType => " & Integer'Image( AltitudeTypeEnum'Enum_Rep (Planning_State.PlanningPosition.AltitudeType)) &
             ", Heading => " & Real64'Image (Real64 (Planning_State.PlanningHeading)));
   end PlanningState_To_String;

   -- Procedure to write PlanningState_Seq to file
   function PlanningState_Seq_To_String
     (Seq  : PlanningState_Seq) return Unbounded_String is
     S : Unbounded_String := To_Unbounded_String ("");
   begin
      for Element of Seq loop
         S := S & PlanningState_To_String (Element);
      end loop;
      return S;
   end PlanningState_Seq_To_String;

   -- Procedure to write Int64_Seq to file
   function Int64_Seq_To_String
     (Seq  : Int64_Seq) return Unbounded_String is
     S : Unbounded_String := To_Unbounded_String ("");
   begin
      for Element of Seq loop
         S := S & Int64'Image (Element);
      end loop;
      return S;
   end Int64_Seq_To_String;

      -- Helper function to convert UniqueAutomationRequest to string
   function UniqueAutomationRequest_To_String
     (Request : UniqueAutomationRequest) return Unbounded_String is
   begin
      return To_Unbounded_String ("UniqueAutomationRequest: RequestID => " & Int64'Image (Request.RequestID) &
         ", OperatingRegion => " & Int64'Image (Request.OperatingRegion) &
         ", RedoAllTasks => " & Boolean'Image (Request.RedoAllTasks)) &
         ", Entity List: => " & Int64_Seq_To_String (Request.EntityList) &
         ", Task List: => " & Int64_Seq_To_String (Request.TaskList) &
         PlanningState_Seq_To_String (Request.PlanningStates) &
         To_Unbounded_String (", SandboxRequest => " & Boolean'Image (Request.SandboxRequest));
   end UniqueAutomationRequest_To_String;

   -- Procedure to write UniqueAutomationRequest_Map to file
   procedure Write_UniqueAutomationRequest_Map
     (File : in File_Type;
      Map  : Int64_UniqueAutomationRequest_Map) is
      use Int64_UAReq_Maps;
      Cu : Cursor := First (Map);
   begin
      while Has_Element (Map, Cu) loop
         Write_String_To_File (File, UniqueAutomationRequest_To_String (Element (Map, Cu)));
         Cu := Next (Map, Cu);
      end loop;
   end Write_UniqueAutomationRequest_Map;

   -- Helper function to convert VehicleAction to string
   function VehicleAction_To_String
     (VA : VehicleAction) return Unbounded_String is
   begin
      return ", VA => " & Int64_Seq_To_String (VA.AssociatedTaskList);
   end VehicleAction_To_String;

   -- Procedure to write VehicleActionCommand_Seq to file
   function VA_Seq_To_String
     (Seq  : VA_Seq) return Unbounded_String is
     S : Unbounded_String := To_Unbounded_String ("");
   begin
      for Element of Seq loop
         S := S & VehicleAction_To_String (Element);
      end loop;
      return S;
   end VA_Seq_To_String;

   -- Helper function to convert VehicleActionCommand to string
   function VehicleActionCommand_To_String
     (Command : VehicleActionCommand) return Unbounded_String is
   begin
      return To_Unbounded_String("VehicleActionCommand: CommandId => " & Int64'Image (Command.CommandId) &
             ", VehicleId => " & Int64'Image (Command.VehicleId)) &
             VA_Seq_To_String (Command.VehicleActionList) &
             To_Unbounded_String(", Status => " & Integer'Image (CommandStatusTypeEnum'Enum_Rep (Command.Status)));
   end VehicleActionCommand_To_String;

      -- Procedure to write VehicleActionCommand_Seq to file
   function VehicleActionCommand_Seq_To_String
     (Seq  : VehicleActionCommand_Seq) return Unbounded_String is
     S : Unbounded_String := To_Unbounded_String ("");
   begin
      for Element of Seq loop
         S := S & VehicleActionCommand_To_String (Element);
      end loop;
      return S;
   end VehicleActionCommand_Seq_To_String;

      -- Helper function to convert UniqueAutomationRequest to string
   function WayPoint_To_String
     (WP : Waypoint) return Unbounded_String is
   begin
      return To_Unbounded_String ("Waypoint: Number => " & Int64'Image (WP.Number) &
         ", Latitude => " & Real64'Image (WP.Latitude) &
         ", Longitude => " & Real64'Image (WP.Longitude) &
         ", Altitude => " & Real64'Image (Real64 (WP.Altitude)) &
         ", AltitudeType => " & Integer'Image (AltitudeTypeEnum'Enum_Rep (WP.AltitudeType)) &
         ", NextWaypoint => " & Int64'Image (WP.NextWaypoint) &
         ", Speed => " & Real64'Image (Real64 (WP.Speed)) &
         ", SpeedType => " & Integer'Image (SpeedTypeEnum'Enum_Rep (WP.SpeedType)) &
         ", ClimbRate => " & Real64'Image (Real64 (WP.ClimbRate)) &
         ", TurnType => " & Integer'Image (TurnTypeEnum'Enum_Rep (WP.TurnType)) &
         ", ContingencyWaypointA => " & Int64'Image (WP.ContingencyWaypointA) &
         ", ContingencyWaypointB => " & Int64'Image (WP.ContingencyWaypointB)) &
         VA_Seq_To_String (WP.VehicleActionList) &
         Int64_Seq_To_String (WP.AssociatedTasks);
   end WayPoint_To_String;

   -- Procedure to write WP_Seq to file
   function WP_Seq_To_String
     (Seq  : WP_Seq) return Unbounded_String is
     S : Unbounded_String := To_Unbounded_String ("");
   begin
      for Element of Seq loop
         S := S & WayPoint_To_String (Element);
      end loop;
      return S;
   end WP_Seq_To_String;

   -- Helper function to convert MissionCommand to string
   function MissionCommand_To_String
     (Command : MissionCommand) return Unbounded_String is
   begin
      return To_Unbounded_String("MissionCommand: CommandId => " & Int64'Image (Command.CommandId) &
             ", VehicleId => " & Int64'Image (Command.VehicleId)) &
             VA_Seq_To_String (Command.VehicleActionList) &
             To_Unbounded_String(", Status => " & Integer'Image (CommandStatusTypeEnum'Enum_Rep (Command.Status))) &
             WP_Seq_To_String (Command.WaypointList) &
            To_Unbounded_String(", FirstWaypoint => " & Int64'Image (Command.FirstWaypoint));
   end MissionCommand_To_String;

   -- Procedure to write MissionCommand_Seq to file
   function MissionCommand_Seq_To_String
     (Seq  : MissionCommand_Seq) return Unbounded_String is
     S : Unbounded_String := To_Unbounded_String ("");
   begin
      for Element of Seq loop
         S := S & MissionCommand_To_String (Element);
      end loop;
      return S;
   end MissionCommand_Seq_To_String;

   -- Procedure to write KVP_Seq to file
   function KVP_Seq_To_String
     (Seq  : KVP_Seq) return Unbounded_String is
     S : Unbounded_String := To_Unbounded_String ("");
   begin
      for Element of Seq loop
         S := S & Element.Key & Element.Value;
      end loop;
      return S;
   end KVP_Seq_To_String;

   -- Helper function to convert UniqueAutomationRequest to string
   function UniqueAutomationResponse_To_String
     (Response : UniqueAutomationResponse) return Unbounded_String is
   begin
      return To_Unbounded_String ("UniqueAutomationResponse: ResponseID => " & Int64'Image (Response.ResponseID)) &
         MissionCommand_Seq_To_String (Response.MissionCommandList) &
         VehicleActionCommand_Seq_To_String (Response.VehicleCommandList) &
         KVP_Seq_To_String (Response.Info) &
         PlanningState_Seq_To_String (Response.FinalStates);
   end UniqueAutomationResponse_To_String;

   -- Procedure to write UniqueAutomationResponse_Map to file
   procedure Write_UniqueAutomationResponse_Map
     (File : in File_Type;
      Map  : Int64_UniqueAutomationResponse_Map) is
      use Int64_UAResp_Maps;
      Cu : Cursor := First (Map);
   begin
      while Has_Element (Map, Cu) loop
         Write_String_To_File (File, UniqueAutomationResponse_To_String (Element(Map, Cu)));
         Cu := Next (Map, Cu);
      end loop;
   end Write_UniqueAutomationResponse_Map;

   -- Helper function to convert TaskAssignment to string
   function TaskAssignment_To_String
     (Task_Assignment : TaskAssignment) return Unbounded_String is
   begin
      return To_Unbounded_String ("TaskAssignment: TaskID => " & Int64'Image (Task_Assignment.TaskID) &
             ", OptionID => " & Int64'Image (Task_Assignment.OptionID) &
             ", AssignedVehicle => " & Int64'Image (Task_Assignment.AssignedVehicle) &
             ", TimeThreshold => " & Int64'Image (Task_Assignment.TimeThreshold) &
             ", TimeTaskCompleted => " & Int64'Image (Task_Assignment.TimeTaskCompleted));
   end TaskAssignment_To_String;

   -- Procedure to write KVP_Seq to file
   function TaskAssignment_Seq_To_String
     (Seq  : TaskAssignment_Sequence) return Unbounded_String is
     S : Unbounded_String := To_Unbounded_String ("");
   begin
      for Element of Seq loop
         S := S & TaskAssignment_To_String (Element);
      end loop;
      return S;
   end TaskAssignment_Seq_To_String;

   -- Helper function to convert TaskAssignment to string
   function TaskAssignmentSummary_To_String
     (Task_Assignment : TaskAssignmentSummary) return Unbounded_String is
   begin
      return To_Unbounded_String ("TaskAssignmentSummary: CorrespondingAutomationRequestID => " & Int64'Image (Task_Assignment.CorrespondingAutomationRequestID) &
             ", OperatingRegion => " & Int64'Image (Task_Assignment.OperatingRegion)) &
             TaskAssignment_Seq_To_String (Task_Assignment.TaskList);
   end TaskAssignmentSummary_To_String;

   -- Procedure to write TaskAssignmentSummary_Map to file
   procedure Write_TaskAssignmentSummary_Map
     (File : in File_Type;
      Map  : Int64_TaskAssignmentSummary_Map) is
      use Int64_TAS_Maps;
      Cu : Cursor := First (Map);
   begin
      while Has_Element (Map, Cu) loop
         Write_String_To_File (File, TaskAssignmentSummary_To_String (Element(Map, Cu)));
         Cu := Next (Map, Cu);
      end loop;
   end Write_TaskAssignmentSummary_Map;


   -- Helper function to convert ProjectedState to string
   function ProjectedState_To_String
     (State : ProjectedState) return Unbounded_String is
   begin
      return To_Unbounded_String ("ProjectedState: FinalWaypointID => " & Int64'Image (State.FinalWaypointID) &
             ", Time => " & Int64'Image (State.Time)) &
             PlanningState_To_String (State.State);
   end ProjectedState_To_String;

   -- Procedure to write ProjectedState_Seq to file
   function ProjectedState_Seq_To_String
     (Seq  : ProjectedState_Seq) return Unbounded_String is
     S : Unbounded_String := To_Unbounded_String ("");
   begin
      for Element of Seq loop
         S := S & ProjectedState_To_String (Element);
      end loop;
      return S;
   end ProjectedState_Seq_To_String;

      -- Procedure to write Int64_ProjectedState_Map to file
   procedure Write_ProjectedState_Map
     (File  : in File_Type;
      ES_Map   : Int64_ProjectedState_Map) is
      use Int64_PS_Maps;
      It_Map : Int64_PS_Maps.Iterable_Map := Iterate (ES_Map);
   begin
      for Key of It_Map loop
         Write_String_To_File (File, ProjectedState_Seq_To_String (Get(ES_Map, Key)));
      end loop;
   end Write_ProjectedState_Map;

      -- Procedure to write Int64_RemainingTaskAssignement_Map to file
   procedure Write_RemainingTaskAssignement_Map
     (File : in File_Type;
      Map  : Int64_RemainingTaskAssignement_Map) is
      use Int64_TAL_Maps;
      Cu : Cursor := First (Map);
   begin
      while Has_Element (Map, Cu) loop
         Write_String_To_File (File, TaskAssignment_Seq_To_String (Element(Map, Cu)));
         Cu := Next (Map, Cu);
      end loop;
   end Write_RemainingTaskAssignement_Map;

   -- Procedure to write Int64_Int64_Map to file
   procedure Write_Int64_Map
     (File : in File_Type;
      Map  : Int64_Int64_Map) is
      use Int64_ER_Maps;
      Cu : Cursor := First (Map);
   begin
      while Has_Element (Map, Cu) loop
         Write_String_To_File (File, To_Unbounded_String (Int64'Image (Element(Map, Cu))));
         Cu := Next (Map, Cu);
      end loop;
   end Write_Int64_Map;

   -- Helper function to convert EntityState to string
   function EntityState_To_String
     (Entity_State : EntityState) return Unbounded_String is
   begin
      return To_Unbounded_String ("EntityState: Id => " & Int64'Image (Entity_State.Id) &
             ", Latitude => " & Real64'Image (Entity_State.Location.Latitude) &
             ", Longitude => " & Real64'Image (Entity_State.Location.Longitude) &
             ", Altitude => " & Real64'Image (Real64 (Entity_State.Location.Altitude)) &
             ", AltitudeType => " & Integer'Image (AltitudeTypeEnum'Enum_Rep (Entity_State.Location.AltitudeType)) &
             ", Heading => " & Real64'Image (Real64 (Entity_State.Heading)) &
             ", Time => " & Int64'Image (Entity_State.Time));
   end EntityState_To_String;

   -- Procedure to write EntityState_Map to file
   procedure Write_EntityState_Map
     (File  : in File_Type;
      ES_Map   : EntityState_Map) is
      use ES_Maps;
      It_Map : ES_Maps.Iterable_Map := Iterate (ES_Map);
   begin
      for Key of It_Map loop
         Write_String_To_File (File, EntityState_To_String (Get(ES_Map, Key)));
      end loop;
   end Write_EntityState_Map;

   -- Helper function to convert EntityState to string
   function SpeedAltPair_To_String
     (SAP : SpeedAltPair) return Unbounded_String is
   begin
      return To_Unbounded_String ("SpeedAltPair: VehicleID => " & Int64'Image (SAP.VehicleID) &
             ", TaskID => " & Int64'Image (SAP.TaskID) &
             ", Speed => " & Real64'Image (Real64 (SAP.Speed)) &
             ", Altitude => " & Real64'Image (Real64 (SAP.Altitude)) &
             ", AltitudeType => " & Integer'Image (AltitudeTypeEnum'Enum_Rep (SAP.AltitudeType)));
   end SpeedAltPair_To_String;

   -- Procedure to write SpeedAltPair_Seq to file
   function SpeedAltPair_Seq_To_String
     (Seq  : SpeedAltPair_Sequence) return Unbounded_String is
     S : Unbounded_String := To_Unbounded_String ("");
   begin
      for Element of Seq loop
         S := S & SpeedAltPair_To_String (Element);
      end loop;
      return S;
   end SpeedAltPair_Seq_To_String;

   -- Procedure to write Int64_ReqeustIDVsOverrides_Map to file
   procedure Write_ReqeustIDVsOverrides_Map
     (File : in File_Type;
      Map  : Int64_ReqeustIDVsOverrides_Map) is
      use Int64_ROR_Maps;
      Cu : Cursor := First (Map);
   begin
      while Has_Element (Map, Cu) loop
         Write_String_To_File (File, SpeedAltPair_Seq_To_String (Element(Map, Cu)));
         Cu := Next (Map, Cu);
      end loop;
   end Write_ReqeustIDVsOverrides_Map;

   -- Main procedure to write the full content of Plan_Builder_State to a file
   procedure Write_State_To_File
     (State     : Plan_Builder_State;
      File_Name : String) is
      File : File_Type;
   begin
      -- Create and open the file
      Create (File, Out_File, File_Name);

      -- Write the content of m_uniqueAutomationRequests
      Write_String_To_File (File, To_Unbounded_String ("m_uniqueAutomationRequests:"));
      Write_UniqueAutomationRequest_Map (File, State.m_uniqueAutomationRequests);

      -- Write the content of UniqueAutomationResponse_Map
      Write_String_To_File (File, To_Unbounded_String ("UniqueAutomationResponse_Map:"));
      Write_UniqueAutomationResponse_Map (File, State.m_inProgressResponse);

      -- Write the content of UniqueAutomationResponse_Map
      Write_String_To_File (File, To_Unbounded_String ("m_assignmentSummaries:"));
      Write_TaskAssignmentSummary_Map (File, State.m_assignmentSummaries);

      -- Write the content of m_projectedEntityStates
      Write_String_To_File (File, To_Unbounded_String ("m_projectedEntityStates:"));
      Write_ProjectedState_Map (File, State.m_projectedEntityStates);

      -- Write the content of m_remainingAssignments
      Write_String_To_File (File, To_Unbounded_String ("m_remainingAssignments:"));
      Write_RemainingTaskAssignement_Map (File, State.m_remainingAssignments);


      -- Write the content of m_remainingAssignments
      Write_String_To_File (File, To_Unbounded_String ("m_expectedResponseID:"));
      Write_Int64_Map (File, State.m_expectedResponseID);

      -- Write the content of EntityState_Map
      Write_String_To_File (File, To_Unbounded_String ("EntityState_Map:"));
      Write_EntityState_Map (File, State.m_currentEntityStates);


      -- Write the content of EntityState_Map
      Write_String_To_File (File, To_Unbounded_String ("m_reqeustIDVsOverrides:"));
      Write_ReqeustIDVsOverrides_Map (File, State.m_reqeustIDVsOverrides);

      -- Close the file
      Close (File);
   end Write_State_To_File;

end State_Serializer;
