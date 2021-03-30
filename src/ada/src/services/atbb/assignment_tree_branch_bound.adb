with Ada.Containers;                     use Ada.Containers;
with Ada.Containers.Formal_Ordered_Maps;
with Ada.Strings.Fixed;                  use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;
with Ada.Strings;                        use Ada.Strings;
with Ada.Text_IO;                        use Ada.Text_IO;
with Algebra;
use Algebra;
with Bounded_Stack;

package body Assignment_Tree_Branch_Bound with SPARK_Mode is

   ---------------------------------------------------
   -- Types used in the computation of the solution --
   ---------------------------------------------------

   type VehicleAssignmentCost is record
      TotalTime         : Int64;
      Last_TaskOptionID : Int64;
   end record
   with Predicate => TotalTime >= 0;

   package Int64_VehicleAssignmentCost_Maps is new Ada.Containers.Formal_Ordered_Maps
     (Key_Type        => Int64,
      Element_Type    => VehicleAssignmentCost);
   use Int64_VehicleAssignmentCost_Maps;
   subtype Int64_VAC_Map is Int64_VehicleAssignmentCost_Maps.Map (200);

   type Assignment_Info is record
      Assignment_Sequence : TaskAssignment_Sequence;
      Vehicle_Assignments : Int64_VAC_Map;
   end record;

   package Assignment_Stack is new Bounded_Stack (Assignment_Info);

   type Stack is new Assignment_Stack.Stack;

   type Children_Arr is array (Positive range <>) of Assignment_Info;

   package Int64_Unbounded_String_Maps is new Ada.Containers.Functional_Maps
     (Key_Type     => Int64,
      Element_Type => Unbounded_String);
   type Int64_Unbounded_String_Map is new Int64_Unbounded_String_Maps.Map;

   -----------------------
   -- Ghost subprograms --
   -----------------------

   function All_Actions_In_Map
   (Algebra             : not null access constant Algebra_Tree_Cell;
      TaskPlanOptions_Map : Int64_TPO_Map)
      return Boolean
   with Ghost;

   function TaskOptionId_In_Map
     (TaskOptionId        : Int64;
      TaskPlanOptions_Map : Int64_TPO_Map)
      return Boolean
   with Ghost;

   function Travel_In_CostMatrix
     (VehicleId, InitTaskOptionId, DestTaskOptionId : Int64;
      Assignment_Cost_Matrix                        : AssignmentCostMatrix)
      return Boolean
   with Ghost;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Children
     (Assignment             : Assignment_Info;
      Algebra                : not null access constant Algebra_Tree_Cell;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Children_Arr
   with
     Pre =>
       Valid_AssignmentCostMatrix (Assignment_Cost_Matrix)
         and then
       Valid_TaskPlanOptions (TaskPlanOptions_Map)
         and then
       All_Actions_In_Map (Algebra, TaskPlanOptions_Map)
         and then
       (for all TOC of Assignment_Cost_Matrix.CostMatrix =>
          Contains (Assignment.Vehicle_Assignments, TOC.VehicleID)),
     Post =>
       (for all Child of Children'Result =>
          (for all TOC of Assignment_Cost_Matrix.CostMatrix =>
             (Contains (Child.Vehicle_Assignments, TOC.VehicleID))));
   --  Returns a sequence of Elements corresponding to all the possible
   --  assignments considering Assignment.

   function Corresponding_TaskOption
     (TaskPlanOptions_Map : Int64_TPO_Map;
      TaskOptionId        : Int64)
      return TaskOption
   with
     Pre =>
       Valid_TaskPlanOptions (TaskPlanOptions_Map)
         and then TaskOptionId_In_Map (TaskOptionId, TaskPlanOptions_Map),
     Post =>
       Corresponding_TaskOption'Result.TaskID = Get_TaskID (TaskOptionId)
         and then Corresponding_TaskOption'Result.OptionID = Get_OptionID (TaskOptionId)
         and then Corresponding_TaskOption'Result.Cost >= 0;
   --  Returns the TaskOption corresponding to TaskOptionId in TaskPlanOptions_Map

   function Corresponding_TaskOptionCost
     (Assignment_Cost_Matrix                        : AssignmentCostMatrix;
      VehicleId, InitTaskOptionId, DestTaskOptionId : Int64)
      return TaskOptionCost
   with
     Pre =>
       Valid_AssignmentCostMatrix (Assignment_Cost_Matrix)
          and then Travel_In_CostMatrix (VehicleId, InitTaskOptionId, DestTaskOptionId, Assignment_Cost_Matrix),
     Post =>
       VehicleId = Corresponding_TaskOptionCost'Result.VehicleID
         and then Get_TaskID (InitTaskOptionId) = Corresponding_TaskOptionCost'Result.InitialTaskID
         and then Get_OptionID (InitTaskOptionId) = Corresponding_TaskOptionCost'Result.InitialTaskOption
         and then Get_TaskID (DestTaskOptionId) = Corresponding_TaskOptionCost'Result.DestinationTaskID
         and then Get_OptionID (DestTaskOptionId) = Corresponding_TaskOptionCost'Result.DestinationTaskOption
         and then Corresponding_TaskOptionCost'Result.TimeToGo >= 0;
   --  Returns the TaskOptionCost corresponding to VehicleId going from
   --  InitTaskOptionId to DestTaskOptionId.

   function Cost (Assignment : Assignment_Info; Cost_Function : Cost_Function_Kind) return Int64;
   --  Returns the cost of an assignment. This function can be expanded to
   --  support other cost functions.

   function Greedy_Solution
     (Data                   : Assignment_Tree_Branch_Bound_Configuration_Data;
      Assignment_Cost_Matrix : AssignmentCostMatrix;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Algebra                : not null access constant Algebra_Tree_Cell)
      return Assignment_Info
   with
     Pre =>
       Valid_AssignmentCostMatrix (Assignment_Cost_Matrix)
         and then
       Valid_TaskPlanOptions (TaskPlanOptions_Map)
         and then
       (for all Id of TaskPlanOptions_Map =>
          (for all TaskOption of Get (TaskPlanOptions_Map, Id).Options => TaskOption.TaskID = Id))
         and then
       All_Actions_In_Map (Algebra, TaskPlanOptions_Map),
     Post =>
       (for all TOC of Assignment_Cost_Matrix.CostMatrix =>
          Contains (Greedy_Solution'Result.Vehicle_Assignments, TOC.VehicleID));
   --  Returns an assignment computed by taking the the child that costs the
   --  less at each iteration. It does not necessarily return the assignment
   --  that minimizes the cost.

   procedure Initialize_Algebra
     (Automation_Request  : UniqueAutomationRequest;
      TaskPlanOptions_Map : Int64_TPO_Map;
      Algebra             : out not null Algebra_Tree)
   with Post => All_Actions_In_Map (Algebra, TaskPlanOptions_Map);
   --  Returns the algebra tree corresponding to the formulas stored in
   --  Automation_Request and the several TaskPlanOptions.

   function Initialize_AssignmentVehicle
     (Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Int64_VAC_Map
   with
     Post =>
       (for all TOC of Assignment_Cost_Matrix.CostMatrix =>
          Contains (Initialize_AssignmentVehicle'Result, TOC.VehicleID));
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
      return Assignment_Info
   with
     Pre =>
       Valid_AssignmentCostMatrix (Assignment_Cost_Matrix)
          and then
       Valid_TaskPlanOptions (TaskPlanOptions_Map)
          and then
       (for some TOC of Assignment_Cost_Matrix.CostMatrix =>
          TOC.VehicleID = VehicleId)
          and then
       (for all TOC of Assignment_Cost_Matrix.CostMatrix =>
          Contains (Assignment.Vehicle_Assignments, TOC.VehicleID))
          and then
       TaskOptionId_In_Map (TaskOptionId, TaskPlanOptions_Map)
          and then
       Travel_In_CostMatrix (VehicleId,
                             Element (Assignment.Vehicle_Assignments, VehicleId).Last_TaskOptionID,
                             TaskOptionId,
                             Assignment_Cost_Matrix),
     Post =>
       (for all TOC of Assignment_Cost_Matrix.CostMatrix =>
          (Contains (New_Assignment'Result.Vehicle_Assignments, TOC.VehicleID)));
   --  This function returns a new Element. It assigns the TaskOptionId to
   --  VehicleId in the enclosing assignment, and computes the new totalTime
   --  of VehicleId.

   ------------------------
   -- All_Actions_In_Map --
   ------------------------

   function All_Actions_In_Map
     (Algebra             : not null access constant Algebra_Tree_Cell;
      TaskPlanOptions_Map : Int64_TPO_Map)
      return Boolean
   is
      (case Algebra.all.Node_Kind is
       when Action =>
         TaskOptionId_In_Map (Algebra.all.TaskOptionId, TaskPlanOptions_Map),
       when Operator =>
         (for all J in 1 .. Algebra.all.Collection.Num_Children =>
            (All_Actions_In_Map (Algebra.all.Collection.Children (J), TaskPlanOptions_Map))),
       when Undefined => False);

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
        or else
          (for some TaskId of Element (State.m_uniqueAutomationRequests, ReqId).TaskList =>
             not Has_Key (Element (State.m_taskPlanOptions, ReqId), TaskId))
      then
         return;
      end if;
      Send_TaskAssignmentSummary (Mailbox, Data, State, ReqId);
   end Check_Assignment_Ready;

   --------------
   -- Children --
   --------------

   function Children
     (Assignment             : Assignment_Info;
      Algebra                : not null access constant Algebra_Tree_Cell;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Children_Arr
   is
      function To_Sequence_Of_TaskOptionId
        (Assignment : Assignment_Info)
         return Int64_Seq;

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
                      TaskAssignment.OptionID));
         end loop;
         return Result;
      end To_Sequence_Of_TaskOptionId;

      Result         : Children_Arr (1 .. 1000);
      Children_Nb    : Natural := 0;
      Objectives_IDs : constant Int64_Seq :=
        Get_Next_Objectives_Ids
          (To_Sequence_Of_TaskOptionId (Assignment),
           Algebra);
      TaskOpt        : TaskOption;
      --  List of TaskOptionIds to be assigned for the next iteration
   begin
      for Objective_ID of Objectives_IDs loop

         pragma Assert (TaskOptionId_In_Map (Objective_ID, TaskPlanOptions_Map));

         TaskOpt := Corresponding_TaskOption (TaskPlanOptions_Map, Objective_ID);

         --  We add a new Assignment to Result for each eligible entity
         --  for Objective_Id.
         for EntityId of TaskOpt.EligibleEntities loop
            pragma Assume (Children_Nb < 1000);
            Children_Nb := Children_Nb + 1;

            pragma Assert
              (Contains (Assignment.Vehicle_Assignments, EntityId));

            pragma Assert
              (Travel_In_CostMatrix
                 (EntityId,
                  Element (Assignment.Vehicle_Assignments, EntityId).Last_TaskOptionID,
                  Objective_ID,
                  Assignment_Cost_Matrix));

            Result (Children_Nb) := New_Assignment (Assignment, EntityId, Objective_ID, Assignment_Cost_Matrix, TaskPlanOptions_Map);

            pragma Loop_Invariant (Children_Nb <= 1000);
            pragma Loop_Invariant
              (for all J in 1 .. Children_Nb =>
                 (for all TOC of Assignment_Cost_Matrix.CostMatrix =>
                      (Contains (Result (J).Vehicle_Assignments, TOC.VehicleID))));
         end loop;

         pragma Loop_Invariant (Children_Nb <= 1000);
         pragma Loop_Invariant
           (for all J in 1 .. Children_Nb =>
              (for all TOC of Assignment_Cost_Matrix.CostMatrix =>
                   (Contains (Result (J).Vehicle_Assignments, TOC.VehicleID))));
      end loop;
      return Result (1 .. Children_Nb);
   end Children;

   ------------------------------
   -- Corresponding_TaskOption --
   ------------------------------

   function Corresponding_TaskOption
     (TaskPlanOptions_Map : Int64_TPO_Map;
      TaskOptionId : Int64)
      return TaskOption
   is
      TaskId           : constant Int64 := Get_TaskID (TaskOptionId);
      OptionId         : constant Int64 := Get_OptionID (TaskOptionId);
      Associated_TPO   : constant TaskPlanOptions := Get (TaskPlanOptions_Map, TaskId);
   begin
      for Pos in TO_Sequences.First .. Last (Associated_TPO.Options) loop

         if Get (Associated_TPO.Options, Pos).OptionID = OptionId then
            return Get (Associated_TPO.Options, Pos);
         end if;

         pragma Loop_Invariant
           (for all J in TO_Sequences.First .. Pos => Get (Associated_TPO.Options, J).OptionID /= OptionId);
      end loop;
      raise Program_Error;
   end Corresponding_TaskOption;

   ----------------------------------
   -- Corresponding_TaskOptionCost --
   ----------------------------------

   function Corresponding_TaskOptionCost
     (Assignment_Cost_Matrix                        : AssignmentCostMatrix;
      VehicleId, InitTaskOptionId, DestTaskOptionId : Int64)
      return TaskOptionCost
   is
      InitialTaskId         : constant Int64 := Get_TaskID (InitTaskOptionId);
      InitialTaskOption     : constant Int64 := Get_OptionID (InitTaskOptionId);
      DestinationTaskId     : constant Int64 := Get_TaskID (DestTaskOptionId);
      DestinationTaskOption : constant Int64 := Get_OptionID (DestTaskOptionId);
   begin
      for Pos in TOC_Sequences.First .. Last (Assignment_Cost_Matrix.CostMatrix) loop
         pragma Loop_Invariant
           (for all J in TOC_Sequences.First .. Pos - 1 =>
              (VehicleId /= Get (Assignment_Cost_Matrix.CostMatrix, J).VehicleID
               or else InitialTaskId /= Get (Assignment_Cost_Matrix.CostMatrix, J).InitialTaskID
               or else InitialTaskOption /= Get (Assignment_Cost_Matrix.CostMatrix, J).InitialTaskOption
               or else DestinationTaskId /= Get (Assignment_Cost_Matrix.CostMatrix, J).DestinationTaskID
               or else DestinationTaskOption /= Get (Assignment_Cost_Matrix.CostMatrix, J).DestinationTaskOption));

         declare
            TOC : constant TaskOptionCost := Get (Assignment_Cost_Matrix.CostMatrix, Pos);
         begin
            if
              VehicleId = TOC.VehicleID
              and then InitialTaskId = TOC.InitialTaskID
              and then InitialTaskOption = TOC.InitialTaskOption
              and then DestinationTaskId = TOC.DestinationTaskID
              and then DestinationTaskOption = TOC.DestinationTaskOption
            then
               return TOC;
            end if;
         end;
      end loop;
      raise Program_Error;
   end Corresponding_TaskOptionCost;

   ----------
   -- Cost --
   ----------

   function Cost (Assignment : Assignment_Info; Cost_Function : Cost_Function_Kind) return Int64 is
      Result : Int64 := 0;
   begin

      case Cost_Function is
         when Minmax =>
            for VehicleID of Assignment.Vehicle_Assignments loop
               declare
                  TotalTime : constant Int64 := Element (Assignment.Vehicle_Assignments, VehicleID).TotalTime;
               begin
                  if TotalTime > Result then
                     Result := TotalTime;
                  end if;
               end;
            end loop;
         when Cumulative =>
            for VehicleId of Assignment.Vehicle_Assignments loop
               pragma Assume (Result < Int64'Last - Element (Assignment.Vehicle_Assignments, VehicleId).TotalTime);
               Result := Result + Element (Assignment.Vehicle_Assignments, VehicleId).TotalTime;
            end loop;
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
      Algebra                : not null access constant Algebra_Tree_Cell)
      return Assignment_Info
   is
      Empty_TA_Seq : TaskAssignment_Sequence;
      Result       : Assignment_Info :=
        (Empty_TA_Seq,
         Initialize_AssignmentVehicle (Assignment_Cost_Matrix));
      Result_Cost  : Int64;
   begin
      while True loop

         pragma Loop_Invariant
           (for all TOC of Assignment_Cost_Matrix.CostMatrix =>
              Contains (Result.Vehicle_Assignments, TOC.VehicleID));

         --  All computed costs will be greater than the current Cost, so
         --  it is assigned to Int64'Last to actually find the Assignment that
         --  minimizes the cost.
         Result_Cost := Int64'Last;
         declare
            Children_A : constant Children_Arr :=
              Children (Result, Algebra, TaskPlanOptions_Map, Assignment_Cost_Matrix);
         begin
            if Children_A'Length = 0 then
               exit;
            else
               for Child of Children_A loop
                  pragma Loop_Invariant
                    (for all TOC of Assignment_Cost_Matrix.CostMatrix =>
                       Contains (Result.Vehicle_Assignments, TOC.VehicleID));

                  declare
                     Current_Cost : constant Int64 := Cost (Child, Data.Cost_Function);
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
      pragma Assume (Length (State.m_assignmentCostMatrixes) < Capacity (State.m_assignmentCostMatrixes));
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
      ReqId : constant Int64 := Options.CorrespondingAutomationRequestID;

      procedure Add_TaskPlanOption
        (Int64_TPO_Map_Map : in out Int64_TaskPlanOptions_Map_Map)
      with
        Pre  =>
          (for all Id of Int64_TPO_Map_Map => Valid_TaskPlanOptions (Element (Int64_TPO_Map_Map, Id)))
            and then Contains (Int64_TPO_Map_Map, ReqId)
            and then not Has_Key (Element (Int64_TPO_Map_Map, ReqId), Options.TaskID)
            and then
              (for all TaskOption of Options.Options =>
                (TaskOption.Cost >= 0
                   and then Options.TaskID = TaskOption.TaskID)),
        Post => (for all Id of Int64_TPO_Map_Map => Valid_TaskPlanOptions (Element (Int64_TPO_Map_Map, Id)));

      procedure Insert_Empty_TPO_Map
        (Int64_TPO_Map_Map : in out Int64_TaskPlanOptions_Map_Map)
      with
        Pre  =>
          (for all Id of Int64_TPO_Map_Map => Valid_TaskPlanOptions (Element (Int64_TPO_Map_Map, Id)))
            and then not Contains (Int64_TPO_Map_Map, ReqId),
        Post =>
          (for all Id of Int64_TPO_Map_Map => Valid_TaskPlanOptions (Element (Int64_TPO_Map_Map, Id)))
            and then Contains (Int64_TPO_Map_Map, ReqId)
            and then not Has_Key (Element (Int64_TPO_Map_Map, ReqId), Options.TaskID);

      ------------------------
      -- Add_TaskPlanOption --
      ------------------------

      procedure Add_TaskPlanOption
        (Int64_TPO_Map_Map : in out Int64_TaskPlanOptions_Map_Map)
      is
         New_Int64_TPO_Map : Int64_TPO_Map;
      begin
         pragma Assert (Valid_TaskPlanOptions (Element (Int64_TPO_Map_Map, ReqId)));

         pragma Assume (Length (Element (Int64_TPO_Map_Map, ReqId)) < Count_Type'Last);
         New_Int64_TPO_Map := Add (Element (Int64_TPO_Map_Map, ReqId), Options.TaskID, Options);
         pragma Assert (Valid_TaskPlanOptions (New_Int64_TPO_Map));

         Replace
           (Int64_TPO_Map_Map,
            ReqId,
            New_Int64_TPO_Map);
         pragma Assert
           (for all Id of Int64_TPO_Map_Map =>
              (if Id /= ReqId then Valid_TaskPlanOptions (Element (Int64_TPO_Map_Map, Id))));
      end Add_TaskPlanOption;

      --------------------------
      -- Insert_Empty_TPO_Map --
      --------------------------

      procedure Insert_Empty_TPO_Map
        (Int64_TPO_Map_Map : in out Int64_TaskPlanOptions_Map_Map)
      is
         Empty_Int64_TPO_Map : Int64_TPO_Map;
      begin
         pragma Assume (Length (Int64_TPO_Map_Map) < Capacity (Int64_TPO_Map_Map));
         Insert (Int64_TPO_Map_Map, ReqId, Empty_Int64_TPO_Map);
      end Insert_Empty_TPO_Map;

   begin
      if not Contains (State.m_taskPlanOptions, ReqId) then
         Insert_Empty_TPO_Map (State.m_taskPlanOptions);
      end if;

      Add_TaskPlanOption (State.m_taskPlanOptions);
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
      pragma Assume (Length (State.m_uniqueAutomationRequests) < Capacity (State.m_uniqueAutomationRequests));
      Insert (State.m_uniqueAutomationRequests, Areq.RequestID, Areq);
      Check_Assignment_Ready (Mailbox, Data, State, Areq.RequestID);
   end Handle_Unique_Automation_Request;

   ------------------------
   -- Initialize_Algebra --
   ------------------------

   procedure Initialize_Algebra
     (Automation_Request  : UniqueAutomationRequest;
      TaskPlanOptions_Map : Int64_TPO_Map;
      Algebra             : out not null Algebra_Tree)
   with SPARK_Mode => Off
   is
      package Unb renames Ada.Strings.Unbounded;

      taskIdVsAlgebraString : Int64_Unbounded_String_Map;
      algebraString         : Unbounded_String := To_Unbounded_String ("");
   begin
      for taskId of TaskPlanOptions_Map loop
         declare
            compositionString              : Unbounded_String :=
              Get (TaskPlanOptions_Map, taskId).Composition;
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
                           positionSpace   : constant Natural :=
                             Unb.Index (compositionString, " ", position);
                           positionParen   : constant Natural :=
                             Unb.Index (compositionString, ")", position);
                        begin
                           if positionSpace /= 0 and then positionParen /= 0 then
                              positionAfterId := Natural'Min (positionSpace, positionParen);
                           else
                              positionAfterId := Natural'Max (positionSpace, positionParen) - 1;
                           end if;

                           declare
                              optionId     : constant Int64 :=
                                Int64'Value (Slice (compositionString, position, positionAfterId));
                              taskOptionId : constant Int64 :=
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

               if Length (TaskRelationShips) > 0 then

                  declare
                     position : Natural := Unb.Index (TaskRelationShips, "p");
                  begin

                     if position > 0 then
                        algebraString :=
                          algebraString &
                          Slice (TaskRelationShips, 1, position - 1);
                        position := position + 1;

                        declare
                           positionAfterId : Natural;
                           positionSpace   : constant Natural :=
                             Unb.Index (TaskRelationShips, " ", position);
                           positionParen   : constant Natural :=
                             Unb.Index (TaskRelationShips, ")", position);
                        begin
                           if positionSpace /= 0 and then positionParen /= 0 then
                              positionAfterId := Natural'Min (positionSpace, positionParen);
                           else
                              positionAfterId := Natural'Max (positionSpace, positionParen);
                           end if;

                           declare
                              taskId : constant Int64 :=
                                Int64'Value (Slice (TaskRelationShips, position, positionAfterId - 1));
                           begin
                              if Has_Key (taskIdVsAlgebraString, taskId) then
                                 algebraString :=
                                   algebraString & Get (taskIdVsAlgebraString, taskId);
                              else
                                 isFinished := True;
                              end if;
                              Delete (TaskRelationShips, 1, positionAfterId - 1);
                           end;
                        end;
                     else
                        algebraString := algebraString & TaskRelationShips;
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
         for taskID of taskIdVsAlgebraString loop
            algebraString :=
              algebraString
              & Get (taskIdVsAlgebraString, taskID)
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
      TOC    : TaskOptionCost;
   begin
      for Index in 1 .. Last (Assignment_Cost_Matrix.CostMatrix) loop
         TOC := Get (Assignment_Cost_Matrix.CostMatrix, Index);

         --  TOC may have several occurences of the same VehicleId
         if not Contains (Result, TOC.VehicleID) then
            pragma Assume (Length (Result) < Result.Capacity);
            Insert (Result, TOC.VehicleID, (0, 0));
         end if;

         pragma Loop_Invariant (for all J in 1 .. Index => Contains (Result, Get (Assignment_Cost_Matrix.CostMatrix, J).VehicleID));
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
      Result             : Assignment_Info;
      Vehicle_Assignment : constant VehicleAssignmentCost :=
        Element (Assignment.Vehicle_Assignments, VehicleId);

      pragma Assume
        (Vehicle_Assignment.TotalTime
         <= Int64'Last - Corresponding_TaskOptionCost (Assignment_Cost_Matrix,
                                                       VehicleId,
                                                       Vehicle_Assignment.Last_TaskOptionID,
                                                       TaskOptionId).TimeToGo);
      TimeThreshold      : constant Int64 :=
        Vehicle_Assignment.TotalTime
        + Corresponding_TaskOptionCost
            (Assignment_Cost_Matrix,
             VehicleId,
             Vehicle_Assignment.Last_TaskOptionID,
             TaskOptionId).TimeToGo;

      pragma Assume
        (TimeThreshold <= Int64'Last - Corresponding_TaskOption (TaskPlanOptions_Map,
                                                                 TaskOptionId).Cost);
      TimeTaskCompleted  : constant Int64 :=
        TimeThreshold
        + Corresponding_TaskOption
             (TaskPlanOptions_Map,
              TaskOptionId).Cost;
   begin
      --  The assignment sequence is the enclosing assignment sequence with
      --  the new TaskAssignment added at the end.
      pragma Assume (Length (Assignment.Assignment_Sequence) < Count_Type'Last);
      Result.Assignment_Sequence :=
        Add (Assignment.Assignment_Sequence,
             (Get_TaskID (TaskOptionId),
              Get_OptionID (TaskOptionId),
              VehicleId,
              TimeThreshold,
              TimeTaskCompleted));

      --  Create the new Vehicle_Assignments map
      Result.Vehicle_Assignments := Assignment.Vehicle_Assignments;

      pragma Assert
        (for all VehicleID of Assignment.Vehicle_Assignments =>
           Contains (Result.Vehicle_Assignments, VehicleID));
      pragma Assert
        (for all TOC of Assignment_Cost_Matrix.CostMatrix =>
           (Contains (Result.Vehicle_Assignments, TOC.VehicleID)));

      -- Only the TimeTotal for VehicleId is modified
      Replace (Result.Vehicle_Assignments, VehicleId, (TimeTaskCompleted, TaskOptionId));

      return Result;
   end New_Assignment;

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

      procedure Bubble_Sort (Arr : in out Children_Arr)
      with
        Pre    =>
           Arr'Length > 0;

      --  Sorts the array of assignments in the ascending order of cost.

      procedure Bubble_Sort (Arr : in out Children_Arr) is
         Switched : Boolean;
      begin
         loop
            Switched := False;

            for J in Arr'First .. Arr'Last - 1 loop
               if Cost (Arr (J + 1), Data.Cost_Function) < Cost (Arr (J), Data.Cost_Function) then
                  declare
                     Tmp : Assignment_Info := Arr (J + 1);
                  begin
                     Arr (J + 1) := Arr (J);
                     Arr (J) := Tmp;
                     Switched := True;
                  end;
               end if;
            end loop;

            exit when not Switched;
         end loop;
      end Bubble_Sort;

      type Min_Option (Found : Boolean := False) is record
         case Found is
            when True =>
               Info : Assignment_Info;
               Cost : Int64;
            when False =>
               null;
         end case;
      end record;

      Algebra         : Algebra_Tree;
      Min             : Min_Option := (Found => False);
      Search_Stack    : Stack;
      Current_Element : Assignment_Info;
      Empty_TA_Seq    : TaskAssignment_Sequence;
      Nodes_Visited   : Int64 := 0;
   begin
      Initialize_Algebra (Automation_Request, TaskPlanOptions_Map, Algebra);
      Put_Line ("Algebra Tree:");
      Print_Tree (Algebra);

      --  The first element is a null assignment

      Push (Search_Stack,
            (Empty_TA_Seq,
             Initialize_AssignmentVehicle (Assignment_Cost_Matrix)));

      --  If the stack is empty, all solutions have been explored

      while Size (Search_Stack) /= 0

      --  We continue at least until we find a solution

        and then (if Min.Found then
                     (Nodes_Visited in 1 .. Data.Number_Nodes_Maximum - 1))
      loop

         --  The element at the top of the stack is popped

         Pop (Search_Stack, Current_Element);

         if not Min.Found or else Cost (Current_Element, Data.Cost_Function) < Min.Cost then
            declare
               Children_A   : Children_Arr :=
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
                  if not Min.Found or else Current_Cost < Min.Cost then
                     Min := (Found => True, Info => Current_Element, Cost => Current_Cost);

                     --  If the maximum number of nodes is 0, we return the first found
                     --  solution, which is the greedy solution.

                     if Data.Number_Nodes_Maximum = 0 then
                        exit;
                     end if;
                  end if;

                  --  Else, we compute the cost for every child and push them into the
                  --  stack if their cost is lower than the current minimal cost.

               else
                  Bubble_Sort (Children_A);
                  for J in reverse Children_A'Range loop
                     declare
                        Child : Assignment_Info := Children_A (J);
                     begin
                        if not Min.Found or else Cost (Child, Data.Cost_Function) < Min.Cost then
                           pragma Assume (Size (Search_Stack) < Assignment_Stack.Capacity, "we have space for another child");
                           Push (Search_Stack, Child);
                        end if;
                     end;
                  end loop;
               end if;
            end;
            Nodes_Visited := Nodes_Visited + 1;
         end if;
      end loop;
      Summary.CorrespondingAutomationRequestID := Automation_Request.RequestID;
      Summary.OperatingRegion := Automation_Request.OperatingRegion;
      Summary.TaskList := Min.Info.Assignment_Sequence;
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
      pragma Assert (for all Id of State.m_taskPlanOptions => Valid_TaskPlanOptions (Element (State.m_taskPlanOptions, Id)));
      Delete (State.m_taskPlanOptions, ReqId);
      pragma Assert (for all Id of State.m_taskPlanOptions => Valid_TaskPlanOptions (Element (State.m_taskPlanOptions, Id)));
   end Send_TaskAssignmentSummary;

   -------------------------
   -- TaskOptionId_In_Map --
   -------------------------

   function TaskOptionId_In_Map
     (TaskOptionId        : Int64;
      TaskPlanOptions_Map : Int64_TPO_Map)
      return Boolean
   is
     (for some TaskId of TaskPlanOptions_Map =>
        (for some TaskOption of Get (TaskPlanOptions_Map, TaskId).Options =>
             (TaskId = TaskOption.TaskID
              and then TaskOption.TaskID = Get_TaskID (TaskOptionId)
              and then TaskOption.OptionID = Get_OptionID (TaskOptionId))));

   function Travel_In_CostMatrix
     (VehicleId, InitTaskOptionId, DestTaskOptionId : Int64;
      Assignment_Cost_Matrix                        : AssignmentCostMatrix)
      return Boolean
   is
     (for some TOC of Assignment_Cost_Matrix.CostMatrix =>
        (VehicleId = TOC.VehicleID
           and then Get_TaskID (InitTaskOptionId) = TOC.InitialTaskID
           and then Get_OptionID (InitTaskOptionId) = TOC.InitialTaskOption
           and then Get_TaskID (DestTaskOptionId) = TOC.DestinationTaskID
           and then Get_OptionID (DestTaskOptionId) = TOC.DestinationTaskOption));
end Assignment_Tree_Branch_Bound;
