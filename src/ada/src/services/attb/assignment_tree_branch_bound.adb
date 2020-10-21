with Ada.Containers; use Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Assignment_Tree_Branch_Bound with SPARK_Mode is

   ---------------------------------------------------
   -- Types used in the computation of the solution --
   ---------------------------------------------------

   type VehicleAssignmentCost is record
      TotalTime         : Int64;
      Last_TaskOptionID : Int64;
   end record;

   package Int64_VehicleAssignmentCost_Maps is new Ada.Containers.Functional_Maps
     (Key_Type     => Int64,
      Element_Type => VehicleAssignmentCost);
   type Int64_VAC_Map is new Int64_VehicleAssignmentCost_Maps.Map;

   type Assignment_Info is record
      Assignment_Sequence : TaskAssignment_Sequence;
      Vehicle_Assignments : Int64_VAC_Map;
   end record;

   package Assignment_Stack is new Bounded_Stack (Assignment_Info);
   use Assignment_Stack;

   type Stack is new Assignment_Stack.Stack;

   type Children_Arr is array (Positive range <>) of Assignment_Info;

   ----------------------
   -- Useful functions --
   ----------------------

   function Corresponding_TaskOption
     (TaskPlanOptions_Map : Int64_TPO_Map;
      TaskOptionId        : Int64)
      return TaskOption;
   --  Returns the TaskOption corresponding to TaskOptionId in TaskPlanOptions_Map

   function Corresponding_TaskOptionCost
     (Assignment_Cost_Matrix                        : AssignmentCostMatrix;
      VehicleId, InitTaskOptionId, DestTaskOptionId : Int64)
      return TaskOptionCost;
   --  Returns the TaskOptionCost corresponding to VehicleId going from
   --  InitTaskOptionId to DestTaskOptionId.

   function Children
     (Assignment             : Assignment_Info;
      Algebra                : access constant Algebra_Tree_Cell;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Children_Arr;
   --  Returns a sequence of Elements corresponding to all the possible
   --  assignments considering Assignment.

   function Cost (Assignment : Assignment_Info; Cost_Function : Cost_Function_Kind) return Int64;
   --  Returns the cost of an assignment. This function can be expanded to
   --  support other cost functions.

   function Greedy_Solution
     (Data                   : Assignment_Tree_Branch_Bound_Configuration_Data;
      Assignment_Cost_Matrix : AssignmentCostMatrix;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Algebra                : access constant Algebra_Tree_Cell)
      return Assignment_Info;
   --  Returns an assignment computed by taking the the child that costs the
   --  less at each iteration. It does not necessarily return the assignment
   --  that minimizes the cost.

   procedure Initialize_Algebra
     (Automation_Request  : UniqueAutomationRequest;
      TaskPlanOptions_Map : Int64_TPO_Map;
      Algebra             : out Algebra_Tree);
   --  Returns the algebra tree corresponding to the formulas stored in
   --  Automation_Request and the several TaskPlanOptions.

   function Initialize_AssignmentVehicle
     (Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Int64_VAC_Map;
   --  Returns the initialized AssignmentVehicle attribute. The keys are the
   --  VehicleIds from the Assignment_Cost_Matrix, and the elements are
   --  - TotalTime = 0
   --  - LastTaskOptionId = VehicleId (travels between the initial location of
   --  a vehicle to a task are stored with InitialTaskOption = VehicleId in
   --  Assignment_Cost_Matrix).

   function New_Assignment
     (Assignment              : Assignment_Info;
      VehicleId, TaskOptionId : Int64;
      Assignment_Cost_Matrix  : AssignmentCostMatrix;
      TaskPlanOptions_Map     : Int64_TPO_Map)
      return Assignment_Info;
   --  This function returns a new Element. It assigns the TaskOptionId to
   --  VehicleId in the enclosing assignment, and computes the new totalTime
   --  of VehicleId.

   ----------------------------
   -- Check_Assignment_Ready --
   ----------------------------

   procedure Check_Assignment_Ready
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      ReqId   : Int64)
   is
   begin
      if not Contains (State.m_uniqueAutomationRequests, ReqId)
        or else not Contains (State.m_assignmentCostMatrixes, ReqId)
        or else not Contains (State.m_taskPlanOptions, ReqId)
      then
         return;
      end if;
      for TaskId of Element (State.m_uniqueAutomationRequests, ReqId).TaskList loop
         if not Has_Key (Element (State.m_taskPlanOptions, ReqId), TaskId) then
            return;
         end if;
      end loop;
      Send_TaskAssignmentSummary (Mailbox, Data, State, ReqId);
   end Check_Assignment_Ready;

   -----------------------------------
   -- Find_Corresponding_TaskOption --
   -----------------------------------

   function Corresponding_TaskOption
     (TaskPlanOptions_Map : Int64_TPO_Map;
      TaskOptionId : Int64)
         return TaskOption
      is
         TaskId           : constant Int64 := Get_TaskID (TaskOptionId);
         OptionId         : constant Int64 := Get_OptionID (TaskOptionId);
         Associated_TPO   : constant TaskPlanOptions := Get (TaskPlanOptions_Map, TaskId);
      begin
      for TaskOption of Associated_TPO.Options loop

            if TaskOption.OptionID = OptionId then
               return TaskOption;
         end if;
      end loop;
      raise Program_Error;
   end Corresponding_TaskOption;

   ---------------------------------------
   -- Find_Corresponding_TaskOptionCost --
   ---------------------------------------

   function Corresponding_TaskOptionCost
     (Assignment_Cost_Matrix                        : AssignmentCostMatrix;
      VehicleId, InitTaskOptionId, DestTaskOptionId : Int64)
      return TaskOptionCost
   is
      InitialTaskId         : Int64 := Get_TaskId (InitTaskOptionId);
      InitialTaskOption     : Int64 := Get_OptionId (InitTaskOptionId);
      DestinationTaskId     : Int64 := Get_TaskId (DestTaskOptionId);
      DestinationTaskOption : Int64 := Get_OptionId (DestTaskOptionId);
   begin
      for TOC of Assignment_Cost_Matrix.CostMatrix loop
         if
           VehicleId = TOC.VehicleId
           and then InitialTaskId = TOC.InitialTaskID
           and then InitialTaskOption = TOC.InitialTaskOption
           and then DestinationTaskId = TOC.DestinationTaskId
           and then DestinationTaskOption = TOC.DestinationTaskOption
         then
            return TOC;
         end if;
      end loop;
      raise Program_Error;
   end Corresponding_TaskOptionCost;

   ------------------------
   -- Initialize_Algebra --
   ------------------------

   package Int64_Unbounded_String_Maps is new Ada.Containers.Functional_Maps
     (Key_Type     => Int64,
      Element_Type => Unbounded_String);
   type Int64_Unbounded_String_Map is new Int64_Unbounded_String_Maps.Map;

   procedure Initialize_Algebra
     (Automation_Request  : UniqueAutomationRequest;
      TaskPlanOptions_Map : Int64_TPO_Map;
      Algebra             : out Algebra_Tree)
   is
      use all type Int64_Unbounded_String_Map;
      package Unb renames Ada.Strings.Unbounded;

      taskIdVsAlgebraString : Int64_Unbounded_String_Map;
      algebraString         : Unbounded_String := To_Unbounded_String ("");
   begin
      for taskId of TaskPlanOptions_Map loop
         declare
            compositionString              : Unbounded_String :=
              Get (TaskPlanOptions_Map, TaskId).Composition;
            algebraCompositionTaskOptionId : Unbounded_String :=
              To_Unbounded_String ("");
            isFinished                     : Boolean := False;
         begin
            while not isFinished loop

               if Length (compositionString) > 0 then

                  declare
                     position : Natural := Unb.Index (compositionString, "p");
                  begin
                     if position > 0 then

                        algebraCompositionTaskOptionId :=
                          algebraCompositionTaskOptionId
                          & Slice (compositionString, 1, position);
                        position := position + 1;

                        declare
                           positionAfterId : Natural;
                           positionSpace   : Natural :=
                             Unb.Index (compositionString, " ", position);
                           positionParen   : Natural :=
                             Unb.Index (compositionString, ")", position);
                        begin
                           if positionSpace /= 0 and then positionParen /= 0 then
                              positionAfterId := Natural'Min (positionSpace, positionParen);
                           else
                              positionAfterId := Natural'Max (positionSpace, positionParen) - 1;
                           end if;

                           declare
                              optionId : Int64 :=
                                Int64'Value (Slice (compositionString, position, positionAfterId));
                              taskOptionId : Int64 :=
                                Get_TaskOptionID (taskId, optionId);
                           begin
                              algebraCompositionTaskOptionId :=
                                algebraCompositionTaskOptionId & Trim (taskOptionId'Image, Side => Left);
                              Delete (compositionString, 1, positionAfterId - 1);
                           end;
                        end;
                     else
                        algebraCompositionTaskOptionId :=
                          algebraCompositionTaskOptionId & compositionString;
                        taskIdVsAlgebraString :=
                          Add (taskIdVsAlgebraString, taskId, algebraCompositionTaskOptionId);
                        isFinished := True;
                     end if;
                  end;
               else
                  isFinished := True;
               end if;
            end loop;
         end;
      end loop;

      if Length (Automation_Request.TaskRelationships) > 0 then
         declare
            isFinished        : Boolean := False;
            TaskRelationShips : Unbounded_String := Automation_Request.TaskRelationships;
         begin
            while not isFinished loop

               if Length (TaskRelationships) > 0 then

                  declare
                     position : Natural := Unb.Index (TaskRelationships, "p");
                  begin

                     if position > 0 then
                        algebraString :=
                          algebraString &
                          Slice (TaskRelationships, 1, position - 1);
                        position := position + 1;

                        declare
                           positionAfterId : Natural;
                           positionSpace   : Natural :=
                             Unb.Index (TaskRelationships, " ", position);
                           positionParen   : Natural :=
                             Unb.Index (TaskRelationships, ")", position);
                        begin
                           if positionSpace /= 0 and then positionParen /= 0 then
                              positionAfterId := Natural'Min (positionSpace, positionParen);
                           else
                              positionAfterId := Natural'Max (positionSpace, positionParen);
                           end if;

                           declare
                              taskId : Int64 :=
                                Int64'Value (Slice (TaskRelationships, position, positionAfterId-1));
                           begin
                              if Has_Key (taskIdVsAlgebraString, taskId) then
                                 algebraString :=
                                   algebraString & Get (taskIdVsAlgebraString, taskId);
                              else
                                 isFinished := True;
                              end if;
                              Delete (TaskRelationships, 1, positionAfterId - 1);
                           end;
                        end;
                     else
                        algebraString := algebraString & TaskRelationships;
                        isFinished := True;
                     end if;
                  end;
               else
                  isFinished := True;
               end if;
            end loop;
         end;
      else
         algebraString := algebraString & "|(";
         for taskID of taskIdvsAlgebraString loop
            algebraString :=
              algebraString
              & Get (taskIdVsAlgebraString, taskId)
              & " ";
         end loop;
         algebraString := algebraString & ")";
      end if;
      Put_Line ("AlgebraString: " & To_String (algebraString));
      Parse_Formula (algebraString, Algebra);
   end Initialize_Algebra;

   ----------------------------------
   -- Initialize_AssignmentVehicle --
   ----------------------------------

   function Initialize_AssignmentVehicle
     (Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Int64_VAC_Map
   is
      Result : Int64_VAC_Map;
   begin
      for TOC of Assignment_Cost_Matrix.CostMatrix loop

         --  TOC may have several occurences of the same VehicleId
         if not Has_Key (Result, TOC.VehicleID) then
            Result := Add (Result, TOC.VehicleID, (0, 0));
         end if;
      end loop;
      return Result;
   end Initialize_AssignmentVehicle;

   --------------------
   -- New_Assignment --
   --------------------

   function New_Assignment
     (Assignment              : Assignment_Info;
      VehicleId, TaskOptionId : Int64;
      Assignment_Cost_Matrix  : AssignmentCostMatrix;
      TaskPlanOptions_Map     : Int64_TPO_Map)
      return Assignment_Info
   is
      Result : Assignment_Info;
      Vehicle_Assignment : VehicleAssignmentCost :=
        Get (Assignment.Vehicle_Assignments, VehicleId);
      TimeThreshold : Int64 :=
        Vehicle_Assignment.TotalTime
        + Corresponding_TaskOptionCost
            (Assignment_Cost_Matrix,
             VehicleId,
             Vehicle_Assignment.Last_TaskOptionID,
             TaskOptionId).TimeToGo;
      TimeTaskCOmpleted : Int64 :=
        TimeThreshold
        + Corresponding_TaskOption
             (TaskPlanOptions_Map,
              TaskOptionId).Cost;
   begin
      --  The assignment sequence is the enclosing assignment sequence with
      --  the new TaskAssignment added at the end.
      Result.Assignment_Sequence :=
        Add (Assignment.Assignment_Sequence,
             (Get_TaskId (TaskOptionID),
              Get_OptionID (TaskOptionId),
              VehicleID,
              TimeThreshold,
              TimeTaskCompleted));

      --  Create the new Vehicle_Assignments map
      for EntityId of Assignment.Vehicle_Assignments loop

         --  If the key corresponds to the vehicle we assign the new task to,
         --  we compute its new total time.
            if EntityId = VehicleID then

               Result.Vehicle_Assignments :=
                 Add (Result.Vehicle_Assignments, EntityId, (TimeTaskCompleted, TaskOptionId));

         --  Otherwise, the total time and last task remains the same
         else
            Result.Vehicle_Assignments :=
              Add (Result.Vehicle_Assignments,
                   EntityId,
                   Get (Assignment.Vehicle_Assignments, EntityId));
         end if;
      end loop;
      return Result;
   end New_Assignment;

   ------------------
   -- Get_Children --
   ------------------

   function Children
     (Assignment             : Assignment_Info;
      Algebra                : access constant Algebra_Tree_Cell;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Children_Arr
   is
      function To_Sequence_Of_TaskOptionId
        (Assignment : Assignment_Info)
         return Int64_Seq
      is
         Result : Int64_Seq;
      begin
         for TaskAssignment of Assignment.Assignment_Sequence loop
            Result :=
              Add (Result,
                   Get_TaskOptionID
                     (TaskAssignment.TaskID,
                      TaskASsignment.OptionID));
         end loop;
         return Result;
      end To_Sequence_Of_TaskOptionId;

      Result         : Children_Arr (1 .. 1000);
      Children_Nb    : Natural := 0;
      Objectives_IDs : Int64_Seq :=
        Get_Next_Objectives_IDs
          (To_Sequence_Of_TaskOptionId (Assignment),
           Algebra);
      --  List of TaskOptionIds to be assigned for the next iteration
   begin
      for Objective_ID of Objectives_IDs loop
         declare
            TaskOpt : TaskOption := Corresponding_TaskOption (TaskPlanOptions_Map, Objective_ID);
         begin

            --  We add a new Assignment to Result for each eligible entity
            --  for Objective_Id.
            for EntityId of TaskOpt.EligibleEntities loop
               Children_Nb := Children_Nb + 1;
               Result (Children_Nb) := New_Assignment (Assignment, EntityId, Objective_ID, Assignment_Cost_Matrix, TaskPlanOptions_Map);
            end loop;
         end;
      end loop;
      return Result (1 .. Children_Nb);
   end Children;

   --------------
   -- Get_Cost --
   --------------

   function Cost (Assignment : Assignment_Info; Cost_Function : Cost_Function_Kind) return Int64 is
      Result : Int64 := 0;
   begin

      case Cost_Function is
         when Minmax =>
            for VehicleID of Assignment.Vehicle_Assignments loop
               declare
                  TotalTime : Int64 := Get (Assignment.Vehicle_Assignments, VehicleID).TotalTime;
               begin
                  if TotalTime > Result then
                     Result := TotalTime;
                  end if;
               end;
            end loop;
         when Cumulative =>
            for VehicleId of Assignment.Vehicle_Assignments loop
               Result := Result + Get (Assignment.Vehicle_Assignments, VehicleID).TotalTime;
            end loop;
         when others =>
            raise Program_Error with "Cost function not implemented for " & Cost_Function'Image;
      end case;
      return Result;
   end Cost;

   ---------------------
   -- Greedy_Solution --
   ---------------------

   function Greedy_Solution
     (Data                   : Assignment_Tree_Branch_Bound_Configuration_Data;
      Assignment_Cost_Matrix : AssignmentCostMatrix;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Algebra                : access constant Algebra_Tree_Cell)
      return Assignment_Info
   is
      Empty_TA_Seq : TaskAssignment_Sequence;
      Result : Assignment_Info :=
        (Empty_TA_Seq,
         Initialize_AssignmentVehicle (Assignment_Cost_Matrix));
      Result_Cost : Int64;
   begin
      while True loop

         --  All computed costs will be greater than the current Cost, so
         --  it is assigned to Int64'Last to actually find the Assignment that
         --  minimizes the cost.
         Result_Cost := Int64'Last;
         declare
            Children_A : Children_Arr :=
              Children (Result, Algebra, TaskPlanOptions_Map, Assignment_Cost_Matrix);
         begin
            if Children_A'Length = 0 then
               exit;
            else
               for Child of Children_A loop
                  declare
                     Current_Cost : Int64 := Cost (Child, Data.Cost_Function);
                  begin

                     if Current_Cost <= Result_Cost then
                        Result := Child;
                        Result_Cost := Current_Cost;
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;
      return Result;
   end Greedy_Solution;

   -----------------------------------
   -- Handle_Assignment_Cost_Matrix --
   -----------------------------------

   procedure Handle_Assignment_Cost_Matrix
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      Matrix  : AssignmentCostMatrix)
   is
   begin
      Insert (State.m_assignmentCostMatrixes, Matrix.CorrespondingAutomationRequestID, Matrix);
      Check_Assignment_Ready (Mailbox, Data, State, Matrix.CorrespondingAutomationRequestID);
   end Handle_Assignment_Cost_Matrix;

   ------------------------------
   -- Handle_Task_Plan_Options --
   ------------------------------

   procedure Handle_Task_Plan_Options
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      Options : TaskPlanOptions)
   is
      ReqId : Int64 := Options.CorrespondingAutomationRequestID;
   begin
      if not Contains (State.m_taskPlanOptions, ReqId) then
         declare
            Empty_Int64_TPO_Map : Int64_TPO_Map;
         begin
            Insert (State.m_taskPlanOptions, ReqId, Empty_Int64_TPO_Map);
         end;
      end if;
      Replace
        (State.m_taskPlanOptions,
         ReqId,
         Add
           (Element (State.m_taskPlanOptions, ReqId),
            Options.TaskId,
            Options));
      Check_Assignment_Ready (Mailbox, Data, State, Options.CorrespondingAutomationRequestID);
   end Handle_Task_Plan_Options;

   --------------------------------------
   -- Handle_Unique_Automation_Request --
   --------------------------------------

   procedure Handle_Unique_Automation_Request
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      Areq    : UniqueAutomationRequest)
   is
   begin
      Insert (State.m_uniqueAutomationRequests, Areq.RequestID, Areq);
      Check_Assignment_Ready (Mailbox, Data, State, Areq.RequestID);
   end Handle_Unique_Automation_Request;

   ------------------------------
   -- Run_Calculate_Assignment --
   ------------------------------

   procedure Run_Calculate_Assignment
     (Data                   : Assignment_Tree_Branch_Bound_Configuration_Data;
      Automation_Request     : UniqueAutomationRequest;
      Assignment_Cost_Matrix : AssignmentCostMatrix;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Summary                : out TaskAssignmentSummary)
   is
      Algebra         : Algebra_Tree;
      Min             : Assignment_Info;
      Min_Cost        : Int64;
      Search_Stack    : Stack;
      Current_Element : Assignment_Info;
      Empty_TA_Seq    : TaskAssignment_Sequence;
      Nodes_Visited   : Int64 := 0;
   begin
      Initialize_Algebra (Automation_Request, TaskPlanOptions_Map, Algebra);
      Min := Greedy_Solution (Data, Assignment_Cost_Matrix, TaskPlanOptions_Map, Algebra);
      Min_Cost := Cost (Min, Data.Cost_Function);
      Put_Line ("Algebra Tree:");
      Print_Tree (Algebra);

      --  The first element is a null assignment
      Push (Search_Stack,
            (Empty_TA_Seq,
             Initialize_AssignmentVehicle (Assignment_Cost_Matrix)));

      while Size (Search_Stack) /= 0 and Nodes_Visited < Data.Number_Nodes_Maximum loop
         --  The element at the top of the stack is popped
         Pop (Search_Stack, Current_Element);
         declare
            Children_A : constant Children_Arr :=
              Children (Current_Element,
                            Algebra,
                            TaskPlanOptions_Map,
                            Assignment_Cost_Matrix);
            Current_Cost : constant Int64 := Cost (Current_Element, Data.Cost_Function);
         begin

            --  If this element has no children, it means that this node
            --  has assigned every task, so we compare it to the current
            --  assignment that minimizes the cost.
            if Children_A'Length = 0 then
               if Current_Cost < Min_Cost then
                  Min := Current_Element;
                  Min_Cost := Current_Cost;
               end if;

            --  Else, we compute the cost for every child and push them into the
            --  stack if their cost is lower than the current minimal cost.
            else
               for J in reverse Children_A'Range loop
                  declare
                     Child : Assignment_Info := Children_A (J);
                  begin
                     if Cost (Child, Data.Cost_Function) < Min_Cost then
                        Push (Search_Stack, Child);
                     end if;
                  end;
               end loop;
            end if;
         end;
         Nodes_Visited := Nodes_Visited + 1;
      end loop;
      Summary.CorrespondingAutomationRequestID := Automation_Request.RequestID;
      Summary.OperatingRegion := Automation_Request.OperatingRegion;
      Summary.TaskList := Min.Assignment_Sequence;
   end Run_Calculate_Assignment;

   ---------------------------------
   --  Send_TaskAssignmentSummary --
   ---------------------------------

   procedure Send_TaskAssignmentSummary
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      ReqId   : Int64)
   is
      Summary : TaskAssignmentSummary;
   begin
      Run_Calculate_Assignment
        (Data,
         Element (State.m_uniqueAutomationRequests, ReqId),
         Element (State.m_assignmentCostMatrixes, ReqId),
         Element (State.m_taskPlanOptions, ReqId),
        Summary);
      sendBroadcastMessage (Mailbox, Summary);
      Delete (State.m_uniqueAutomationRequests, ReqId);
      Delete (State.m_assignmentCostMatrixes, ReqId);
      Delete (State.m_taskPlanOptions, ReqId);
   end Send_TaskAssignmentSummary;

end Assignment_Tree_Branch_Bound;
