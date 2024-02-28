with Ada.Containers;                     use Ada.Containers;
with Ada.Strings.Fixed;                  use Ada.Strings.Fixed;
with Ada.Strings;                        use Ada.Strings;
with Ada.Text_IO;                        use Ada.Text_IO;
with Algebra;                            use Algebra;
with Bounded_Stack;
with Int64_Parsing;                      use Int64_Parsing;

package body Assignment_Tree_Branch_Bound with SPARK_Mode is

   ---------------------------------------------------
   -- Types used in the computation of the solution --
   ---------------------------------------------------

   type VehicleAssignmentCost is record
      TotalTime       : Int64;
      Last_TaskOption : TaskOption;
   end record
   with Predicate => TotalTime >= 0;

   package Int64_VehicleAssignmentCost_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
     (Key_Type     => Int64,
      Element_Type => VehicleAssignmentCost,
      Hash         => Int64_Hash);
   use Int64_VehicleAssignmentCost_Maps;
   subtype Int64_VAC_Map is Int64_VehicleAssignmentCost_Maps.Map (Int64_VehicleAssignmentCost_Maps.Default_Modulus (10));
   package Int64_VehicleAssignmentCost_Maps_P renames Int64_VehicleAssignmentCost_Maps.Formal_Model.P;
   package Int64_VehicleAssignmentCost_Maps_K renames Int64_VehicleAssignmentCost_Maps.Formal_Model.K;

   type Assignment_Info is record
      Assignment_Sequence : TaskAssignment_Sequence;
      Vehicle_Assignments : Int64_VAC_Map;
   end record;

   type Children_Arr is array (Positive range <>) of Assignment_Info;

   package Int64_Unbounded_String_Maps is new SPARK.Containers.Functional.Maps
     (Key_Type     => Int64,
      Element_Type => Unbounded_String);
   type Int64_Unbounded_String_Map is new Int64_Unbounded_String_Maps.Map;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Children
     (Assignment             : Assignment_Info;
      Algebra                : not null access constant Algebra_Tree_Cell;
      Automation_Request     : UniqueAutomationRequest;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Children_Arr
   with
     Pre =>
       Valid_AssignmentCostMatrix (Assignment_Cost_Matrix)
         and then
       Valid_TaskPlanOptions (TaskPlanOptions_Map)
         and then
       Valid_Assignment (Assignment, TaskPlanOptions_Map, Automation_Request)
         and then
       All_Actions_In_Map (Algebra, TaskPlanOptions_Map)
         and then
       All_EligibleEntities_In_EntityList (Automation_Request, TaskPlanOptions_Map)
         and then
       All_Travels_In_CostMatrix (Automation_Request, TaskPlanOptions_Map, Assignment_Cost_Matrix),
     Post =>
        (for all Child of Children'Result => Valid_Assignment (Child, TaskPlanOptions_Map, Automation_Request));
   --  Returns a sequence of Elements corresponding to all the possible
   --  assignments considering Assignment.

   function Corresponding_TaskOption
     (TaskPlanOptions_Map : Int64_TPO_Map;
      TaskOptionId        : Int64)
      return TaskOption
   with
     Pre =>
       Valid_TaskPlanOptions (TaskPlanOptions_Map)
         and then TaskOptionId in 0 .. 9_999_999_999
         and then TaskOptionId_In_Map (TaskOptionId, TaskPlanOptions_Map),
     Post =>
       (for some TaskId of TaskPlanOptions_Map =>
          (for some Option of Get (TaskPlanOptions_Map, TaskId).Options =>
             Corresponding_TaskOption'Result = Option))
         and then Corresponding_TaskOption'Result.TaskID = Get_TaskID (TaskOptionId)
         and then Corresponding_TaskOption'Result.OptionID = Get_OptionID (TaskOptionId)
         and then Get_TaskOptionID (Corresponding_TaskOption'Result.TaskID, Corresponding_TaskOption'Result.OptionID) = TaskOptionId
         and then Corresponding_TaskOption'Result.Cost >= 0;
   --  Returns the TaskOption corresponding to TaskOptionId in TaskPlanOptions_Map

   function Corresponding_TaskOptionCost
     (Assignment_Cost_Matrix : AssignmentCostMatrix;
      VehicleId              : Int64;
      DestTaskOption         : TaskOption)
      return TaskOptionCost
   with
     Pre =>
       Valid_AssignmentCostMatrix (Assignment_Cost_Matrix)
          and then Travel_In_CostMatrix (VehicleId, DestTaskOption, Assignment_Cost_Matrix),
     Post =>
       VehicleId = Corresponding_TaskOptionCost'Result.VehicleID
         and then 0 = Corresponding_TaskOptionCost'Result.InitialTaskID
         and then 0 = Corresponding_TaskOptionCost'Result.InitialTaskOption
         and then DestTaskOption.TaskID = Corresponding_TaskOptionCost'Result.DestinationTaskID
         and then DestTaskOption.OptionID = Corresponding_TaskOptionCost'Result.DestinationTaskOption
         and then Corresponding_TaskOptionCost'Result.TimeToGo >= 0;

   function Corresponding_TaskOptionCost
     (Assignment_Cost_Matrix         : AssignmentCostMatrix;
      VehicleId                      : Int64;
      InitTaskOption, DestTaskOption : TaskOption)
      return TaskOptionCost
   with
     Pre =>
       Valid_AssignmentCostMatrix (Assignment_Cost_Matrix)
          and then Travel_In_CostMatrix (VehicleId, InitTaskOption, DestTaskOption, Assignment_Cost_Matrix),
     Post =>
       VehicleId = Corresponding_TaskOptionCost'Result.VehicleID
         and then InitTaskOption.TaskID = Corresponding_TaskOptionCost'Result.InitialTaskID
         and then InitTaskOption.OptionID = Corresponding_TaskOptionCost'Result.InitialTaskOption
         and then DestTaskOption.TaskID = Corresponding_TaskOptionCost'Result.DestinationTaskID
         and then DestTaskOption.OptionID = Corresponding_TaskOptionCost'Result.DestinationTaskOption
         and then Corresponding_TaskOptionCost'Result.TimeToGo >= 0;
   --  Returns the TaskOptionCost corresponding to VehicleId going from
   --  InitTaskOptionId to DestTaskOptionId.

   function Cost (Assignment : Assignment_Info; Cost_Function : Cost_Function_Kind) return Int64;
   --  Returns the cost of an assignment. This function can be expanded to
   --  support other cost functions.

   procedure Initialize_Algebra
     (Automation_Request  : UniqueAutomationRequest;
      TaskPlanOptions_Map : Int64_TPO_Map;
      Algebra             : out Algebra_Tree;
      Error               : out Boolean;
      Message             : out Unbounded_String)
   with Post => (if not Error then Algebra /= null and then All_Actions_In_Map (Algebra, TaskPlanOptions_Map));
   --  Returns the algebra tree corresponding to the formulas stored in
   --  Automation_Request and the several TaskPlanOptions.

   function New_Assignment
     (Assignment             : Assignment_Info;
      VehicleId              : Int64;
      TaskOpt                : TaskOption;
      Assignment_Cost_Matrix : AssignmentCostMatrix;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Automation_Request     : UniqueAutomationRequest)
      return Assignment_Info
   with
     Pre =>
         Valid_AssignmentCostMatrix (Assignment_Cost_Matrix)
           and then
         Valid_TaskPlanOptions (TaskPlanOptions_Map)
           and then
         Valid_Assignment (Assignment, TaskPlanOptions_Map, Automation_Request)
           and then TaskOpt.TaskID in 0 .. 99_999
           and then TaskOpt.OptionID in 0 .. 99_999
           and then
         (for some TOC of Assignment_Cost_Matrix.CostMatrix => TOC.VehicleID = VehicleId)
           and then
         (for some TaskId of TaskPlanOptions_Map =>
            (declare
               Options : TaskOption_Seq renames Get (TaskPlanOptions_Map, TaskId).Options;
             begin
                Contains (Options, TO_Sequences.First, Last (Options), TaskOpt)
                  and then Is_Eligible (Automation_Request, TaskOpt, VehicleId)))
           and then
         (if Contains (Assignment.Vehicle_Assignments, VehicleId)
          then Travel_In_CostMatrix (VehicleId, Element (Assignment.Vehicle_Assignments, VehicleId).Last_TaskOption, TaskOpt, Assignment_Cost_Matrix)
          else Travel_In_CostMatrix (VehicleId, TaskOpt, Assignment_Cost_Matrix)),
     Post =>
       Valid_Assignment (New_Assignment'Result, TaskPlanOptions_Map, Automation_Request);
   --  This function returns a new Element. It assigns the TaskOptionId to
   --  VehicleId in the enclosing assignment, and computes the new totalTime
   --  of VehicleId.

   -----------------------
   -- Ghost subprograms --
   -----------------------

   function All_Actions_In_Map
     (Algebra             : not null access constant Algebra_Tree_Cell;
      TaskPlanOptions_Map : Int64_TPO_Map)
      return Boolean
   with Ghost,
        Pre => True,
        Subprogram_Variant => (Structural => Algebra);
   pragma Annotate (GNATprove, Always_Return, All_Actions_In_Map);

   function TaskOptionId_In_Map
     (TaskOptionId        : Int64;
      TaskPlanOptions_Map : Int64_TPO_Map)
      return Boolean
   with Ghost, Pre => TaskOptionId in 0 .. 9_999_999_999;

   function Valid_Assignment
     (Assignment          : Assignment_Info;
      TaskPlanOptions_Map : Int64_TPO_Map;
      Automation_Request  : UniqueAutomationRequest)
      return Boolean
   with Ghost, Pre => Valid_TaskPlanOptions (TaskPlanOptions_Map), Post => True;

   ------------------------
   -- Useful subprograms --
   ------------------------

   function Contains_Corresponding_TaskOption
     (Assignment_Sequence : TaskAssignment_Sequence;
      TaskOpt             : TaskOption)
      return Boolean
   is
     (for some TaskAssignment of Assignment_Sequence =>
        (TaskAssignment.TaskID = TaskOpt.TaskID
         and then TaskAssignment.OptionID = TaskOpt.OptionID));

   function Has_Corresponding_Option
     (Automation_Request : UniqueAutomationRequest;
      Options            : TaskOption_Seq;
      TaskOpt            : TaskOption;
      EntityId           : Int64)
      return Boolean
   is
     (Contains (Options, TO_Sequences.First, Last (Options), TaskOpt)
        and then
      Is_Eligible (Automation_Request, TaskOpt, EntityId));

   function Contains_Corresponding_TaskOption
      (Automation_Request  : UniqueAutomationRequest;
       TaskPlanOptions_Map : Int64_TPO_Map;
       TaskOpt             : TaskOption;
       EntityId            : Int64)
       return Boolean
   is
     (for some TaskID of TaskPlanOptions_Map =>
        (Has_Corresponding_Option
             (Automation_Request,
              Get (TaskPlanOptions_Map, TaskID).Options,
              TaskOpt,
              EntityId)));

   procedure Equal_TaskOpt_Lemma
     (Automation_Request   : UniqueAutomationRequest;
      TaskPlanOptions_Map  : Int64_TPO_Map;
      TaskOpt_1, TaskOpt_2 : TaskOption;
      EntityId             : Int64)
   with
     Ghost,
     Pre  =>
       Contains_Corresponding_TaskOption (Automation_Request, TaskPlanOptions_Map, TaskOpt_1, EntityId)
         and then TaskOpt_1 = TaskOpt_2,
     Post => Contains_Corresponding_TaskOption (Automation_Request, TaskPlanOptions_Map, TaskOpt_2, EntityId);

   procedure Equal_TaskOpt_Lemma
     (Automation_Request   : UniqueAutomationRequest;
      TaskPlanOptions_Map  : Int64_TPO_Map;
      TaskOpt_1, TaskOpt_2 : TaskOption;
      EntityId             : Int64)
   is null;

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
         Algebra.all.TaskOptionId in 0 .. 9_999_999_999
           and then
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
      Automation_Request     : UniqueAutomationRequest;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Children_Arr
   is

      procedure Prove_TaskOpt_Different_From_Last_TaskOption
        (EntityId : Int64;
         TaskOpt  : TaskOption)
      with
        Ghost,
        Pre  =>
          Valid_TaskPlanOptions (TaskPlanOptions_Map)
            and then
          Valid_Assignment (Assignment, TaskPlanOptions_Map, Automation_Request)
            and then
          TaskOpt.TaskID in 0 .. 99_999
            and then
          TaskOpt.OptionID in 0 .. 99_999
            and then
          not Contains (To_Sequence_Of_TaskOptionId (Assignment),
                        TO_Sequences.First,
                        Last (To_Sequence_Of_TaskOptionId (Assignment)),
                        Get_TaskOptionID (TaskOpt.TaskID, TaskOpt.OptionID)),
        Post =>
          (if Contains (Assignment.Vehicle_Assignments, EntityId)
           then
             (TaskOpt /= Element (Assignment.Vehicle_Assignments, EntityId).Last_TaskOption
                and then
             (for some TaskId of TaskPlanOptions_Map =>
                (declare
                   Options         : TaskOption_Seq renames Get (TaskPlanOptions_Map, TaskId).Options;
                   Last_TaskOption : TaskOption renames Element (Assignment.Vehicle_Assignments, EntityId).Last_TaskOption;
                 begin
                   Contains (Options, TO_Sequences.First, Last (Options), Last_TaskOption)
                     and then Is_Eligible (Automation_Request, Last_TaskOption, EntityId)))));

      procedure Prove_TaskOptionId_In_Map
        (ID  : Int64;
         Alg : not null access constant Algebra_Tree_Cell)
      with
        Ghost,
        Pre  => Is_Present (Alg, ID) and then All_Actions_In_Map (Alg, TaskPlanOptions_Map),
        Post => ID in 0 .. 9_999_999_999 and then TaskOptionId_In_Map (ID, TaskPlanOptions_Map),
        Subprogram_Variant => (Structural => Alg);
      pragma Annotate (GNATprove, Always_Return, Prove_TaskOptionId_In_Map);

      procedure Prove_Travel_In_CostMatrix
        (EntityId : Int64;
         TaskOpt  : TaskOption)
      with
        Ghost,
        Pre  =>
          All_Travels_In_CostMatrix (Automation_Request, TaskPlanOptions_Map, Assignment_Cost_Matrix)
            and then
          (for some TaskId of TaskPlanOptions_Map =>
             (declare
                Options : TaskOption_Seq renames Get (TaskPlanOptions_Map, TaskId).Options;
              begin
                Contains (Options, TO_Sequences.First, Last (Options), TaskOpt)
                  and then Is_Eligible (Automation_Request, TaskOpt, EntityId)))
            and then
          Contains (Automation_Request.EntityList, TO_Sequences.First, Last (Automation_Request.EntityList), EntityId)
            and then
          Valid_TaskPlanOptions (TaskPlanOptions_Map)
            and then
          (if Contains (Assignment.Vehicle_Assignments, EntityId)
           then
             (TaskOpt /= Element (Assignment.Vehicle_Assignments, EntityId).Last_TaskOption
                and then
             (for some TaskId of TaskPlanOptions_Map =>
                (declare
                   Options         : TaskOption_Seq renames Get (TaskPlanOptions_Map, TaskId).Options;
                   Last_TaskOption : TaskOption renames Element (Assignment.Vehicle_Assignments, EntityId).Last_TaskOption;
                 begin
                   Contains (Options, TO_Sequences.First, Last (Options), Last_TaskOption)
                     and then Is_Eligible (Automation_Request, Last_TaskOption, EntityId))))),
        Post =>
            (if not Contains (Assignment.Vehicle_Assignments, EntityId)
             then Travel_In_CostMatrix (EntityId,
                                        TaskOpt,
                                        Assignment_Cost_Matrix)
             else Travel_In_CostMatrix (EntityId,
                                        Element (Assignment.Vehicle_Assignments, EntityId).Last_TaskOption,
                                        TaskOpt,
                                        Assignment_Cost_Matrix));

      function To_Sequence_Of_TaskOptionId
        (Assignment : Assignment_Info)
         return Int64_Seq
      with
        Pre  =>
          (for all TaskAssignment of Assignment.Assignment_Sequence =>
             (TaskAssignment.TaskID in 0 .. 99_999
                and then
              TaskAssignment.OptionID in 0 .. 99_999)),
        Post =>
          (for all TaskAssignment of Assignment.Assignment_Sequence =>
             (for some TaskOptionId of To_Sequence_Of_TaskOptionId'Result =>
                 (Get_TaskOptionID (TaskAssignment.TaskID, TaskAssignment.OptionID) = TaskOptionId)));

      --------------------------------------------------
      -- Prove_TaskOpt_Different_From_Last_TaskOption --
      --------------------------------------------------

      procedure Prove_TaskOpt_Different_From_Last_TaskOption
        (EntityId : Int64;
         TaskOpt  : TaskOption)
      is
      begin
         if Contains (Assignment.Vehicle_Assignments, EntityId) then
            declare
               Last_TaskOption : constant TaskOption := Element (Assignment.Vehicle_Assignments, EntityId).Last_TaskOption;
            begin
               pragma Assert
                 (for some TaskId of TaskPlanOptions_Map =>
                    (declare
                       Options         : TaskOption_Seq renames Get (TaskPlanOptions_Map, TaskId).Options;
                       Last_TaskOption : TaskOption renames Element (Assignment.Vehicle_Assignments, EntityId).Last_TaskOption;
                     begin
                       Contains (Options, TO_Sequences.First, Last (Options), Last_TaskOption)
                         and then Is_Eligible (Automation_Request, Last_TaskOption, EntityId)));
               pragma Assert
                 (not Contains (To_Sequence_Of_TaskOptionId (Assignment),
                                TO_Sequences.First,
                                Last (To_Sequence_Of_TaskOptionId (Assignment)),
                                Get_TaskOptionID (TaskOpt.TaskID, TaskOpt.OptionID)));
               pragma Assert
                 (for some TaskAssignment of Assignment.Assignment_Sequence =>
                    (TaskAssignment.TaskID = Last_TaskOption.TaskID
                     and then TaskAssignment.OptionID = Last_TaskOption.OptionID
                     and then Get_TaskOptionID (TaskAssignment.TaskID, TaskAssignment.OptionID)
                              = Get_TaskOptionID (Last_TaskOption.TaskID, Last_TaskOption.OptionID)));
               pragma Assert
                 (Contains
                    (To_Sequence_Of_TaskOptionId (Assignment),
                     TO_Sequences.First,
                     Last (To_Sequence_Of_TaskOptionId (Assignment)),
                     Get_TaskOptionID (Last_TaskOption.TaskID, Last_TaskOption.OptionID)));
            end;
         end if;
      end Prove_TaskOpt_Different_From_Last_TaskOption;

      -------------------------------
      -- Prove_TaskOptionId_In_Map --
      -------------------------------

      procedure Prove_TaskOptionId_In_Map
        (ID  : Int64;
         Alg : not null access constant Algebra_Tree_Cell) is
      begin
         case Alg.Node_Kind is
            when Action   =>
               pragma Assert (ID = Alg.TaskOptionId);
               pragma Assert (TaskOptionId_In_Map (Alg.all.TaskOptionId, TaskPlanOptions_Map));
               pragma Assert (TaskOptionId_In_Map (ID, TaskPlanOptions_Map));
            when Operator =>
               for J in 1 .. Alg.Collection.Num_Children loop
                  pragma Loop_Invariant
                    (for all K in 1 .. J - 1 =>
                       not Is_Present (Alg.Collection.Children (K), ID));
                  pragma Loop_Invariant
                    (for some K in J .. Alg.Collection.Num_Children =>
                       Is_Present (Alg.Collection.Children (K), ID));

                  if Is_Present (Alg.Collection.Children (J), ID) then
                     Prove_TaskOptionId_In_Map (ID, Alg.Collection.Children (J));
                     exit;
                  end if;
               end loop;
            when Undefined =>
               raise Program_Error;
         end case;
      end Prove_TaskOptionId_In_Map;

      --------------------------------
      -- Prove_Travel_In_CostMatrix --
      --------------------------------

      procedure Prove_Travel_In_CostMatrix
        (EntityId : Int64;
         TaskOpt  : TaskOption)
      is

      begin
         if not Contains (Assignment.Vehicle_Assignments, EntityId) then
            pragma Assert
              (for all TaskId of TaskPlanOptions_Map =>
                 (for all Option of Get (TaskPlanOptions_Map, TaskId).Options =>
                      (if Is_Eligible (Automation_Request, Option, EntityId)
                       then Travel_In_CostMatrix (EntityId, Option, Assignment_Cost_Matrix))));
         else
            declare
               Last_Option : constant TaskOption := Element (Assignment.Vehicle_Assignments, EntityId).Last_TaskOption;
            begin
               pragma Assert
                 (for all TaskId of TaskPlanOptions_Map =>
                    (for all Option of Get (TaskPlanOptions_Map, TaskId).Options =>
                       (if Is_Eligible (Automation_Request, Option, EntityId)
                        then
                          (for all TaskId_2 of TaskPlanOptions_Map =>
                             (for all Option_2 of Get (TaskPlanOptions_Map, TaskId_2).Options =>
                                (if Option /= Option_2 and then Is_Eligible (Automation_Request, Option_2, EntityId)
                                 then Travel_In_CostMatrix (EntityId,
                                                            Option,
                                                            Option_2,
                                                            Assignment_Cost_Matrix)))))));
               pragma Assert
                 (for some TaskId of TaskPlanOptions_Map =>
                    (declare
                       Options : TaskOption_Seq renames Get (TaskPlanOptions_Map, TaskId).Options;
                     begin
                       Contains (Options, TO_Sequences.First, Last (Options), Last_Option)
                         and then Is_Eligible (Automation_Request, Last_Option, EntityId)
                         and then
                           (for all TaskId_2 of TaskPlanOptions_Map =>
                              (for all Option_2 of Get (TaskPlanOptions_Map, TaskId_2).Options =>
                                 (if Last_Option /= Option_2 and then Is_Eligible (Automation_Request, Option_2, EntityId)
                                  then Travel_In_CostMatrix (EntityId,
                                                             Last_Option,
                                                             Option_2,
                                                             Assignment_Cost_Matrix))))));
               pragma Assert
                 (for all TaskId of TaskPlanOptions_Map =>
                    (for all Option of Get (TaskPlanOptions_Map, TaskId).Options =>
                       (if Option /= Last_Option and then Is_Eligible (Automation_Request, Option, EntityId)
                        then Travel_In_CostMatrix (EntityId,
                                                   Last_Option,
                                                   Option,
                                                   Assignment_Cost_Matrix))));
               pragma Assert
                 (Travel_In_CostMatrix (EntityId, Element (Assignment.Vehicle_Assignments, EntityId).Last_TaskOption, TaskOpt, Assignment_Cost_Matrix));
            end;
         end if;
      end Prove_Travel_In_CostMatrix;

      ---------------------------------
      -- To_Sequence_Of_TaskOptionId --
      ---------------------------------

      function To_Sequence_Of_TaskOptionId
        (Assignment : Assignment_Info)
         return Int64_Seq
      is
         use all type TaskAssignment_Sequence;
         Result : Int64_Seq;
      begin
         for J in TO_Sequences.First .. Last (Assignment.Assignment_Sequence) loop
            pragma Assume (Length (Result) < Count_Type'Last);
            Result :=
              Add (Result,
                   Get_TaskOptionID
                     (Get (Assignment.Assignment_Sequence, J).TaskID,
                      Get (Assignment.Assignment_Sequence, J).OptionID));
            pragma Loop_Invariant
              (for all K in TO_Sequences.First .. J =>
                 (for some TaskOptionID of Result =>
                    (TaskOptionID = Get_TaskOptionID (Get (Assignment.Assignment_Sequence, K).TaskID, Get (Assignment.Assignment_Sequence, K).OptionID))));
         end loop;
         return Result;
      end To_Sequence_Of_TaskOptionId;

      Result         : Children_Arr (1 .. 500);
      Children_Nb    : Natural := 0;
      Objectives_IDs : constant Int64_Seq :=
        Get_Next_Objectives_Ids
          (To_Sequence_Of_TaskOptionId (Assignment),
           Algebra);
      TaskOpt        : TaskOption;
      --  List of TaskOptionIds to be assigned for the next iteration
   begin
      for Objective_ID of Objectives_IDs loop
         Prove_TaskOptionId_In_Map (Objective_ID, Algebra);

         pragma Assert (TaskOptionId_In_Map (Objective_ID, TaskPlanOptions_Map));

         TaskOpt := Corresponding_TaskOption (TaskPlanOptions_Map, Objective_ID);

         --  We add a new Assignment to Result for each eligible entity
         --  for Objective_Id.
         for EntityId of TaskOpt.EligibleEntities loop
            pragma Assume (Children_Nb < 500);
            Children_Nb := Children_Nb + 1;

            Prove_TaskOpt_Different_From_Last_TaskOption (EntityId, TaskOpt);

            Prove_Travel_In_CostMatrix (EntityId, TaskOpt);

            Result (Children_Nb) := New_Assignment (Assignment, EntityId, TaskOpt, Assignment_Cost_Matrix, TaskPlanOptions_Map, Automation_Request);

            pragma Loop_Invariant (Children_Nb <= 500);
            pragma Loop_Invariant (for all J in 1 .. Children_Nb => Valid_Assignment (Result (J), TaskPlanOptions_Map, Automation_Request));
         end loop;

         pragma Loop_Invariant (Children_Nb <= 500);
         pragma Loop_Invariant (for all J in 1 .. Children_Nb => Valid_Assignment (Result (J), TaskPlanOptions_Map, Automation_Request));
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
     (Assignment_Cost_Matrix : AssignmentCostMatrix;
      VehicleId              : Int64;
      DestTaskOption         : TaskOption)
      return TaskOptionCost
   is
   begin
      for Pos in TOC_Sequences.First .. Last (Assignment_Cost_Matrix.CostMatrix) loop
         pragma Loop_Invariant
           (for all J in TOC_Sequences.First .. Pos - 1 =>
              (VehicleId /= Get (Assignment_Cost_Matrix.CostMatrix, J).VehicleID
               or else 0 /= Get (Assignment_Cost_Matrix.CostMatrix, J).InitialTaskID
               or else 0 /= Get (Assignment_Cost_Matrix.CostMatrix, J).InitialTaskOption
               or else DestTaskOption.TaskID /= Get (Assignment_Cost_Matrix.CostMatrix, J).DestinationTaskID
               or else DestTaskOption.OptionID /= Get (Assignment_Cost_Matrix.CostMatrix, J).DestinationTaskOption));

         declare
            TOC : constant TaskOptionCost := Get (Assignment_Cost_Matrix.CostMatrix, Pos);
         begin
            if
              VehicleId = TOC.VehicleID
              and then 0 = TOC.InitialTaskID
              and then 0 = TOC.InitialTaskOption
              and then DestTaskOption.TaskID = TOC.DestinationTaskID
              and then DestTaskOption.OptionID = TOC.DestinationTaskOption
            then
               return TOC;
            end if;
         end;
      end loop;
      raise Program_Error;
   end Corresponding_TaskOptionCost;

   ----------------------------------
   -- Corresponding_TaskOptionCost --
   ----------------------------------

   function Corresponding_TaskOptionCost
     (Assignment_Cost_Matrix         : AssignmentCostMatrix;
      VehicleId                      : Int64;
      InitTaskOption, DestTaskOption : TaskOption)
      return TaskOptionCost
   is
   begin
      for Pos in TOC_Sequences.First .. Last (Assignment_Cost_Matrix.CostMatrix) loop
         pragma Loop_Invariant
           (for all J in TOC_Sequences.First .. Pos - 1 =>
              (VehicleId /= Get (Assignment_Cost_Matrix.CostMatrix, J).VehicleID
               or else InitTaskOption.TaskID /= Get (Assignment_Cost_Matrix.CostMatrix, J).InitialTaskID
               or else InitTaskOption.OptionID /= Get (Assignment_Cost_Matrix.CostMatrix, J).InitialTaskOption
               or else DestTaskOption.TaskID /= Get (Assignment_Cost_Matrix.CostMatrix, J).DestinationTaskID
               or else DestTaskOption.OptionID /= Get (Assignment_Cost_Matrix.CostMatrix, J).DestinationTaskOption));

         declare
            TOC : constant TaskOptionCost := Get (Assignment_Cost_Matrix.CostMatrix, Pos);
         begin
            if
              VehicleId = TOC.VehicleID
              and then InitTaskOption.TaskID = TOC.InitialTaskID
              and then InitTaskOption.OptionID = TOC.InitialTaskOption
              and then DestTaskOption.TaskID = TOC.DestinationTaskID
              and then DestTaskOption.OptionID = TOC.DestinationTaskOption
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
      pragma Assume (Length (State.m_assignmentCostMatrixes) < Count_Type'Last, "we have space for another assignment cost matrix");
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
        (assignmentCostMatrixes   : Int64_AssignmentCostMatrix_Map;
         taskPlanOptions          : in out Int64_TaskPlanOptions_Map_Map;
         uniqueAutomationRequests : Int64_UniqueAutomationRequest_Map)
      with
        Annotate => (GNATprove, Always_Return),
        Pre  =>
            (for all Req of taskPlanOptions =>
               (Valid_TaskPlanOptions (Element (taskPlanOptions, Req))
                  and then Contains (uniqueAutomationRequests, Req)
                  and then
                All_EligibleEntities_In_EntityList
                  (Element (uniqueAutomationRequests, Req),
                   Element (taskPlanOptions, Req))))
               and then
            (for all Req of assignmentCostMatrixes =>
               Valid_AssignmentCostMatrix (Element (assignmentCostMatrixes, Req))
                 and then Contains (uniqueAutomationRequests, Req)
                 and then Contains (taskPlanOptions, Req)
                 and then
                   All_Travels_In_CostMatrix
                     (Element (uniqueAutomationRequests, Req),
                      Element (taskPlanOptions, Req),
                      Element (assignmentCostMatrixes, Req)))
               and then
             Contains (taskPlanOptions, ReqId)
               and then
             not Has_Key (Element (taskPlanOptions, ReqId), Options.TaskID)
               and then
             (for all TaskOption of Options.Options =>
                (TaskOption.Cost >= 0
                   and then Options.TaskID = TaskOption.TaskID)),
        Post =>
          (for all Req of taskPlanOptions =>
             (Valid_TaskPlanOptions (Element (taskPlanOptions, Req))
                and then Contains (uniqueAutomationRequests, Req)
                and then
                  All_EligibleEntities_In_EntityList
                    (Element (uniqueAutomationRequests, Req),
                     Element (taskPlanOptions, Req))))
             and then
          (for all Req of assignmentCostMatrixes =>
             Valid_AssignmentCostMatrix (Element (assignmentCostMatrixes, Req))
               and then Contains (uniqueAutomationRequests, Req)
               and then Contains (taskPlanOptions, Req)
               and then
                 All_Travels_In_CostMatrix
                   (Element (uniqueAutomationRequests, Req),
                    Element (taskPlanOptions, Req),
                    Element (assignmentCostMatrixes, Req)));

      ------------------------
      -- Add_TaskPlanOption --
      ------------------------

      procedure Add_TaskPlanOption
        (assignmentCostMatrixes   : Int64_AssignmentCostMatrix_Map;
         taskPlanOptions          : in out Int64_TaskPlanOptions_Map_Map;
         uniqueAutomationRequests : Int64_UniqueAutomationRequest_Map)
      is
         pragma SPARK_Mode (Off);
         New_Int64_TPO_Map : Int64_TPO_Map;
      begin
         New_Int64_TPO_Map := Add (Element (taskPlanOptions, ReqId), Options.TaskID, Options);

         Replace
           (taskPlanOptions,
            ReqId,
            New_Int64_TPO_Map);
      end Add_TaskPlanOption;

   begin
      if not Contains (State.m_taskPlanOptions, ReqId) then
         pragma Assume (Length (State.m_taskPlanOptions) < Count_Type'Last, "we have space for another map");
         Insert (State.m_taskPlanOptions, ReqId, Empty_Map);
      end if;
      Add_TaskPlanOption (State.m_assignmentCostMatrixes, State.m_taskPlanOptions, State.m_uniqueAutomationRequests);
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
      pragma Assume (Length (State.m_uniqueAutomationRequests) < Count_Type'Last, "we have space for another request");
      Insert (State.m_uniqueAutomationRequests, Areq.RequestID, Areq);
      Check_Assignment_Ready (Mailbox, Data, State, Areq.RequestID);
   end Handle_Unique_Automation_Request;

   ------------------------
   -- Initialize_Algebra --
   ------------------------

   procedure Initialize_Algebra
     (Automation_Request  : UniqueAutomationRequest;
      TaskPlanOptions_Map : Int64_TPO_Map;
      Algebra             : out Algebra_Tree;
      Error               : out Boolean;
      Message             : out Unbounded_String)
   is
      package Unb renames Common.Unbounded_Strings_Subprograms;

      taskIdVsAlgebraString : Int64_Unbounded_String_Map;
      algebraString         : Unbounded_String := To_Unbounded_String ("");
   begin
      Algebra := null;
      Message := Null_Unbounded_String;
      Error   := False;

      for M in Iterate (TaskPlanOptions_Map) loop
         pragma Loop_Variant (Decreases => Int64_TaskPlanOptions_Maps.Length (M));
         declare
            taskId : constant Int64 := Int64_TaskPlanOptions_Maps.Choose (M);
         begin
            if taskId not in 0 .. 99_999 then
               Append_To_Msg (Message, "TaskID ");
               Append_To_Msg (Message, Print_Int64 (taskId));
               Append_To_Msg (Message, " should be in range 0 .. 99_999.");
               Error := True;
               return;
            end if;

            declare
               compositionString              : Unbounded_String :=
                 Get (TaskPlanOptions_Map, taskId).Composition;
               algebraCompositionTaskOptionId : Unbounded_String :=
                 To_Unbounded_String ("");
               isFinished                     : Boolean := False;
            begin
               if Length (compositionString) = Natural'Last then
                  Append_To_Msg (Message, "Composition string of TaskID ");
                  Append_To_Msg (Message, Print_Int64 (taskId));
                  Append_To_Msg (Message, " is too long.");
                  Error := True;
                  return;
               end if;
               while not isFinished loop
                  pragma Loop_Invariant (Length (compositionString) < Natural'Last);
                  if Length (compositionString) > 0 then

                     declare
                        position : Natural := Unb.Index (compositionString, "p");
                     begin
                        if position > 0 then
                           if Length (algebraCompositionTaskOptionId) >= Natural'Last - position then
                              Append_To_Msg (Message, "Composition string of TaskID ");
                              Append_To_Msg (Message, Print_Int64 (taskId));
                              Append_To_Msg (Message, " is too long.");
                              Error := True;
                              return;
                           end if;
                           algebraCompositionTaskOptionId :=
                             algebraCompositionTaskOptionId
                             & Unb.Slice (compositionString, 1, position);

                           declare
                              positionAfterId : Natural;
                              positionSpace   : constant Natural :=
                                Unb.Index (compositionString, " ", position);
                              positionParen   : constant Natural :=
                                Unb.Index (compositionString, ")", position);
                           begin
                              if positionSpace = 0 and then positionParen = 0 then
                                 Append_To_Msg (Message, "Substring " & '"');
                                 Append_To_Msg (Message, Unb.Slice (compositionString, position, Length (compositionString)));
                                 Append_To_Msg (Message, '"' & ": optionID after character 'p' should be followed by character ' ' or ')'.");
                                 Error := True;
                                 return;
                              elsif positionSpace /= 0 and then positionParen /= 0 then
                                 positionAfterId := Natural'Min (positionSpace, positionParen);
                              else
                                 positionAfterId := Natural'Max (positionSpace, positionParen);
                              end if;

                              if positionAfterId - 1 < position + 1 then
                                 Append_To_Msg (Message, "Substring " & '"');
                                 Append_To_Msg (Message, Unb.Slice (compositionString, position, Length (compositionString)));
                                 Append_To_Msg (Message, '"' & ": character 'p' should be followed by an optionID.");
                                 Error := True;
                                 return;
                              end if;

                              declare
                                 optionId, taskOptionId : Int64;
                                 Parsing_Error          : Boolean;
                              begin
                                 Parse_Int64 (Unb.Slice (compositionString, position + 1, positionAfterId - 1), optionId, Parsing_Error);

                                 if Parsing_Error then
                                    Append_To_Msg (Message, "Substring " & '"');
                                    Append_To_Msg (Message, Unb.Slice (compositionString, position + 1, positionAfterId - 1));
                                    Append_To_Msg (Message, '"' & ": does not correspond to an Int64.");
                                    Error := True;
                                    return;
                                 end if;

                                 if optionId not in 0 .. 99_999 then
                                    Append_To_Msg (Message, "OptionID ");
                                    Append_To_Msg (Message, Print_Int64 (optionId));
                                    Append_To_Msg (Message, " should be in range 0 .. 99_999.");
                                    Error := True;
                                    return;
                                 end if;

                                 taskOptionId := Get_TaskOptionID (taskId, optionId);

                                 declare
                                    Image : String := Print_Int64 (taskOptionId);
                                 begin
                                    if Length (algebraCompositionTaskOptionId) >= Natural'Last - Image'Length then
                                       Append_To_Msg (Message, "Composition string of TaskID ");
                                       Append_To_Msg (Message, Print_Int64 (taskId));
                                       Append_To_Msg (Message, " is too long.");
                                       Error := True;
                                       return;
                                    end if;
                                    algebraCompositionTaskOptionId :=
                                      algebraCompositionTaskOptionId & Image;
                                 end;
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
         end;
      end loop;

      if Length (Automation_Request.TaskRelationships) > 0 then
         declare
            isFinished        : Boolean := False;
            TaskRelationships : Unbounded_String := Automation_Request.TaskRelationships;
         begin
            if Length (TaskRelationships) = Natural'Last then
               Append_To_Msg (Message, "TaskRelationships string is too long.");
               Error := True;
               return;
            end if;
            while not isFinished loop
               pragma Loop_Invariant (Length (TaskRelationships) < Natural'Last);

               if Length (TaskRelationships) > 0 then

                  declare
                     position : Natural := Unb.Index (TaskRelationships, "p");
                  begin

                     if position > 0 then
                        if Length (algebraString) >= Natural'Last - position + 1 then
                           Append_To_Msg (Message, "Algebra string is too long.");
                           Error := True;
                           return;
                        end if;
                        algebraString :=
                          algebraString &
                          Unb.Slice (TaskRelationships, 1, position - 1);

                        declare
                           positionAfterId : Natural;
                           positionSpace   : constant Natural :=
                             Unb.Index (TaskRelationships, " ", position);
                           positionParen   : constant Natural :=
                             Unb.Index (TaskRelationships, ")", position);
                        begin
                           if positionSpace = 0 and then positionParen = 0 then
                              Append_To_Msg (Message, "Substring " & '"');
                              Append_To_Msg (Message, Slice (TaskRelationships, position, Length (TaskRelationships)));
                              Append_To_Msg (Message, '"' & ": taskID after character 'p' should be followed by character ' ' or ')'.");
                              Error := True;
                              return;
                           elsif positionSpace /= 0 and then positionParen /= 0 then
                              positionAfterId := Natural'Min (positionSpace, positionParen);
                           else
                              positionAfterId := Natural'Max (positionSpace, positionParen);
                           end if;

                           if positionAfterId - 1 < position + 1 then
                              Append_To_Msg (Message, "Substring " & '"');
                              Append_To_Msg (Message, Unb.Slice (TaskRelationships, position, Length (TaskRelationships)));
                              Append_To_Msg (Message, '"' & ": character 'p' should be followed by an optionID.");
                              Error := True;
                              return;
                           end if;

                           declare
                              taskId        : Int64;
                              Parsing_Error : Boolean;
                           begin
                              Parse_Int64 (Unb.Slice (TaskRelationships, position + 1, positionAfterId - 1), taskId, Parsing_Error);

                              if Parsing_Error then
                                 Append_To_Msg (Message, "Substring " & '"');
                                 Append_To_Msg (Message, Unb.Slice (TaskRelationships, position + 1, positionAfterId - 1));
                                 Append_To_Msg (Message, '"' & ": does not correspond to an Int64.");
                                 Error := True;
                                 return;
                              end if;

                              if taskId not in 0 .. 99_999 then
                                 Append_To_Msg (Message, "TaskID ");
                                 Append_To_Msg (Message, Print_Int64 (taskId));
                                 Append_To_Msg (Message, " should be in range 0 .. 99_999.");
                                 Error := True;
                                 return;
                              end if;

                              if Has_Key (taskIdVsAlgebraString, taskId) then
                                 if Length (algebraString) > Natural'Last - Length (Get (taskIdVsAlgebraString, taskId)) then
                                    Append_To_Msg (Message, "Algebra string is too long.");
                                    Error := True;
                                    return;
                                 end if;
                                 algebraString :=
                                   algebraString & Get (taskIdVsAlgebraString, taskId);
                              else
                                 Append_To_Msg (Message, "TaskID ");
                                 Append_To_Msg (Message, Print_Int64 (taskId));
                                 Append_To_Msg (Message, " does not exist.");
                                 Error := True;
                                 return;
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
         for taskID of Iterate (taskIdVsAlgebraString) loop
            if Length (algebraString) >= Natural'Last - 1 - Length (Get (taskIdVsAlgebraString, taskID)) then
               Append_To_Msg (Message, "Algebra string is too long.");
               Error := True;
               return;
            end if;
            algebraString :=
              algebraString
              & Get (taskIdVsAlgebraString, taskID)
              & " ";
         end loop;
         algebraString := algebraString & ")";
         pragma Assert (Length (algebraString) > 0);
      end if;
      Put ("AlgebraString: ");
      Put_Line (To_String (algebraString));
      if Length (algebraString) <= 1 then
         Append_To_Msg (Message, "Algebra string is too short.");
         Error := True;
         return;
      end if;

      if not Error then
         Parse_Formula (algebraString, Algebra, Error, Message);

         if not Error then
            declare
               function Action_In_TaskPlanOptions_Map (TaskOptionId : Int64)
                                                       return Boolean
               with
                 Pre  => TaskOptionId in 0 .. 9_999_999_999,
                 Post =>
                   (not Action_In_TaskPlanOptions_Map'Result)
                     = (for all TaskId of TaskPlanOptions_Map =>
                          (for all TaskOption of Get (TaskPlanOptions_Map, TaskId).Options =>
                             (TaskId /= TaskOption.TaskID
                              or else TaskOption.TaskID /= Get_TaskID (TaskOptionId)
                              or else TaskOption.OptionID /= Get_OptionID (TaskOptionId))));

               procedure Check_Actions_In_Map_Rec (Tree : not null access constant Algebra_Tree_Cell) with
                 Post => (if not Error then All_Actions_In_Map (Tree, TaskPlanOptions_Map)),
                 Subprogram_Variant => (Structural => Tree),
                 Annotate => (GNATprove, Always_Return);

               function Action_In_TaskPlanOptions_Map (TaskOptionId : Int64)
                                                       return Boolean
               is
                  TaskId  : Int64;
                  TaskOpt : TaskOption;
                  use all type Int64_TPO_Map;
               begin
                  for M in Iterate (TaskPlanOptions_Map) loop
                     pragma Loop_Invariant
                       (for all TaskI of TaskPlanOptions_Map =>
                          (if not Int64_TaskPlanOptions_Maps.Has_Key (M, TaskI) then
                             (for all TaskOption of Get (TaskPlanOptions_Map, TaskI).Options =>
                               (TaskI /= TaskOption.TaskID
                                or else TaskOption.TaskID /= Get_TaskID (TaskOptionId)
                                or else TaskOption.OptionID /= Get_OptionID (TaskOptionId)))));
                     TaskId := Int64_TaskPlanOptions_Maps.Choose (M);
                     for J in TO_Sequences.First .. Last (Get (TaskPlanOptions_Map, TaskId).Options) loop
                        TaskOpt := Get (Get (TaskPlanOptions_Map, TaskId).Options, J);
                        if TaskId = TaskOpt.TaskID
                          and then TaskOpt.TaskID = Get_TaskID (TaskOptionId)
                          and then TaskOpt.OptionID = Get_OptionID (TaskOptionId)
                        then
                           return True;
                        end if;
                        pragma Loop_Invariant
                          (for all K in TO_Sequences.First .. J =>
                             (declare
                              TempTaskOpt : constant TaskOption := Get (Get (TaskPlanOptions_Map, TaskId).Options, K);
                              begin
                              TaskId /= TempTaskOpt.TaskID
                              or else TempTaskOpt.TaskID /= Get_TaskID (TaskOptionId)
                              or else TempTaskOpt.OptionID /= Get_OptionID (TaskOptionId)));
                     end loop;
                  end loop;
                  return False;
               end Action_In_TaskPlanOptions_Map;

               procedure Check_Actions_In_Map_Rec (Tree : not null access constant Algebra_Tree_Cell) is
               begin
                  case Tree.all.Node_Kind is
                  when Action =>
                     if Tree.TaskOptionId not in 0 .. 9_999_999_999 then
                        Append_To_Msg (Message, "TaskOptionId ");
                        Append_To_Msg (Message, Print_Int64 (Tree.TaskOptionId));
                        Append_To_Msg (Message, " should be in range 0 .. 9_999_999_999.");
                        Error := True;
                        return;
                     end if;

                     if not Action_In_TaskPlanOptions_Map (Tree.TaskOptionId) then
                        Append_To_Msg (Message, "OptionId ");
                        Append_To_Msg (Message, Print_Int64 (Get_OptionID (Tree.TaskOptionId)));
                        Append_To_Msg (Message, " does not exist for TaskId ");
                        Append_To_Msg (Message, Print_Int64 (Get_TaskID (Tree.TaskOptionId)));
                        Append_To_Msg (Message, '.');
                        Error := True;
                        return;
                     end if;
                  when Operator =>
                     for J in 1 .. Tree.Collection.Num_Children loop
                        Check_Actions_In_Map_Rec (Tree.Collection.Children (J));

                        if Error then
                           return;
                        end if;

                        pragma Loop_Invariant (for all K in 1 .. J => All_Actions_In_Map (Tree.Collection.Children (K), TaskPlanOptions_Map));
                     end loop;
                  when Undefined =>
                     Append_To_Msg (Message, "Algebra tree is not well formed.");
                     Error := True;
                     return;
                  end case;
               end Check_Actions_In_Map_Rec;
            begin
               Check_Actions_In_Map_Rec (Algebra);
            end;
         end if;
      end if;
   end Initialize_Algebra;

   --------------------
   -- New_Assignment --
   --------------------

   function New_Assignment
     (Assignment              : Assignment_Info;
      VehicleId               : Int64;
      TaskOpt                 : TaskOption;
      Assignment_Cost_Matrix  : AssignmentCostMatrix;
      TaskPlanOptions_Map     : Int64_TPO_Map;
      Automation_Request      : UniqueAutomationRequest)
      return Assignment_Info
   is
      Result             : Assignment_Info;
      Vehicle_Assignment : constant VehicleAssignmentCost :=
        (if Contains (Assignment.Vehicle_Assignments, VehicleId)
         then Element (Assignment.Vehicle_Assignments, VehicleId)
         else VehicleAssignmentCost'(0, TaskOpt));

      pragma Assume
        (Vehicle_Assignment.TotalTime
         <= Int64'Last
            - (if Contains (Assignment.Vehicle_Assignments, VehicleId)
               then Corresponding_TaskOptionCost (Assignment_Cost_Matrix,
                                                  VehicleId,
                                                  Vehicle_Assignment.Last_TaskOption,
                                                   TaskOpt).TimeToGo
               else Corresponding_TaskOptionCost (Assignment_Cost_Matrix,
                                                  VehicleId,
                                                  TaskOpt).TimeToGo));

      TimeThreshold      : constant Int64 :=
        Vehicle_Assignment.TotalTime
        + (if Contains (Assignment.Vehicle_Assignments, VehicleId)
               then Corresponding_TaskOptionCost (Assignment_Cost_Matrix,
                                                  VehicleId,
                                                  Vehicle_Assignment.Last_TaskOption,
                                                   TaskOpt).TimeToGo
               else Corresponding_TaskOptionCost (Assignment_Cost_Matrix,
                                                  VehicleId,
                                                  TaskOpt).TimeToGo);

      pragma Assume
        (TimeThreshold <= Int64'Last - TaskOpt.Cost);
      TimeTaskCompleted  : constant Int64 :=
        TimeThreshold
        + TaskOpt.Cost;

      procedure Prove_Final_Value_Is_Valid with
        Ghost,
        Pre  =>
          Valid_TaskPlanOptions (TaskPlanOptions_Map)
            and then Valid_Assignment (Assignment, TaskPlanOptions_Map, Automation_Request)
            and then Length (Assignment.Assignment_Sequence) < Count_Type'Last
            and then TaskOpt.TaskID in 0 .. 99_999
            and then TaskOpt.OptionID in 0 .. 99_999
            and then Result.Assignment_Sequence = Add (Assignment.Assignment_Sequence,
                                                       (TaskOpt.TaskID,
                                                        TaskOpt.OptionID,
                                                        VehicleId,
                                                        TimeThreshold,
                                                        TimeTaskCompleted))
            and then
              (for some TaskAssignment of Result.Assignment_Sequence =>
                 (TaskAssignment.TaskID = TaskOpt.TaskID and then TaskAssignment.OptionID = TaskOpt.OptionID))
            and then
              Contains_Corresponding_TaskOption (Automation_Request, TaskPlanOptions_Map, TaskOpt, VehicleId)
            and then
              (for all EntityId of Result.Vehicle_Assignments =>
                 (if EntityId /= VehicleId
                  then Contains (Assignment.Vehicle_Assignments, EntityId)
                         and then Element (Result.Vehicle_Assignments, EntityId) = Element (Assignment.Vehicle_Assignments, EntityId)
                  else Element (Result.Vehicle_Assignments, EntityId).Last_TaskOption = TaskOpt)),
        Post => Valid_Assignment (Result, TaskPlanOptions_Map, Automation_Request);

      procedure Prove_Initial_Value_Is_Valid with
        Ghost,
        Pre  =>
          Length (Assignment.Assignment_Sequence) < Count_Type'Last
            and then TaskOpt.TaskID in 0 .. 99_999
            and then TaskOpt.OptionID in 0 .. 99_999
            and then
          Result.Assignment_Sequence = Add (Assignment.Assignment_Sequence,
                                            (TaskOpt.TaskID,
                                             TaskOpt.OptionID,
                                             VehicleId,
                                             TimeThreshold,
                                             TimeTaskCompleted))
            and then Assignment.Vehicle_Assignments = Result.Vehicle_Assignments
            and then Valid_TaskPlanOptions (TaskPlanOptions_Map)
            and then Valid_Assignment (Assignment, TaskPlanOptions_Map, Automation_Request),
        Post =>
          Valid_Assignment (Result, TaskPlanOptions_Map, Automation_Request);

      --------------------------------
      -- Prove_Final_Value_Is_Valid --
      --------------------------------

      procedure Prove_Final_Value_Is_Valid is
         I : Int64_VehicleAssignmentCost_Maps.Cursor := First (Result.Vehicle_Assignments);
         use Int64_VehicleAssignmentCost_Maps.Formal_Model;
      begin

         while Has_Element (Result.Vehicle_Assignments, I) loop
            if Key (Result.Vehicle_Assignments, I) = VehicleId then
               pragma Assert
                 (Contains_Corresponding_TaskOption (Automation_Request, TaskPlanOptions_Map, TaskOpt, Key (Result.Vehicle_Assignments, I)));
               Equal_TaskOpt_Lemma (Automation_Request,
                                    TaskPlanOptions_Map,
                                    TaskOpt,
                                    Element (Result.Vehicle_Assignments, I).Last_TaskOption,
                                    Key (Result.Vehicle_Assignments, I));
            else
               pragma Assert
                 (Contains (Assignment.Vehicle_Assignments, Key (Result.Vehicle_Assignments, I))
                    and then Element (Assignment.Vehicle_Assignments, Key (Result.Vehicle_Assignments, I))
                             = Element (Result.Vehicle_Assignments, I));
               pragma Assert
                 (Contains_Corresponding_TaskOption
                    (Assignment.Assignment_Sequence,
                     Element (Assignment.Vehicle_Assignments, Key (Result.Vehicle_Assignments, I)).Last_TaskOption));
               pragma Assert
                 (Contains_Corresponding_TaskOption
                    (Automation_Request,
                     TaskPlanOptions_Map,
                     Element (Assignment.Vehicle_Assignments, Key (Result.Vehicle_Assignments, I)).Last_TaskOption,
                     Key (Result.Vehicle_Assignments, I)));

               Equal_TaskOpt_Lemma (Automation_Request,
                                    TaskPlanOptions_Map,
                                    Element (Assignment.Vehicle_Assignments, Key (Result.Vehicle_Assignments, I)).Last_TaskOption,
                                    Element (Result.Vehicle_Assignments, I).Last_TaskOption,
                                    Key (Result.Vehicle_Assignments, I));
            end if;

            pragma Loop_Invariant (Has_Element (Result.Vehicle_Assignments, I));
            pragma Loop_Invariant
              (for all K in 1 .. Int64_VehicleAssignmentCost_Maps_P.Get (Positions (Result.Vehicle_Assignments), I) =>
                 (Contains_Corresponding_TaskOption
                    (Result.Assignment_Sequence,
                     Element (Result.Vehicle_Assignments,
                              Int64_VehicleAssignmentCost_Maps_K.Get (Keys (Result.Vehicle_Assignments), K)).Last_TaskOption)
                    and then
                  (Contains_Corresponding_TaskOption
                     (Automation_Request,
                      TaskPlanOptions_Map,
                      Element (Result.Vehicle_Assignments,
                               Int64_VehicleAssignmentCost_Maps_K.Get (Keys (Result.Vehicle_Assignments), K)).Last_TaskOption,
                      Int64_VehicleAssignmentCost_Maps_K.Get (Keys (Result.Vehicle_Assignments), K)))));
            Next (Result.Vehicle_Assignments, I);
         end loop;

         for J in TO_Sequences.First .. Last (Result.Assignment_Sequence) loop
            if J /= Last (Result.Assignment_Sequence) then
               pragma Assert (Get (Result.Assignment_Sequence, J) = Get (Assignment.Assignment_Sequence, J));
               pragma Assert (Get (Result.Assignment_Sequence, J).TaskID in 0 .. 99_999
                                and then Get (Result.Assignment_Sequence, J).OptionID in 0 .. 99_999);
            else
               pragma Assert (Get (Result.Assignment_Sequence, J).TaskID = TaskOpt.TaskID
                                and then Get (Result.Assignment_Sequence, J).OptionID = TaskOpt.OptionID);
               pragma Assert (Get (Result.Assignment_Sequence, J).TaskID in 0 .. 99_999
                                and then Get (Result.Assignment_Sequence, J).OptionID in 0 .. 99_999);
            end if;

            pragma Loop_Invariant
              (for all K in TO_Sequences.First .. J =>
                 (Get (Result.Assignment_Sequence, K).TaskID in 0 .. 99_999
                  and then Get (Result.Assignment_Sequence, K).OptionID in 0 .. 99_999));
         end loop;
      end Prove_Final_Value_Is_Valid;

      ----------------------------------
      -- Prove_Initial_Value_Is_Valid --
      ----------------------------------

      procedure Prove_Initial_Value_Is_Valid is
         I : Int64_VehicleAssignmentCost_Maps.Cursor := First (Result.Vehicle_Assignments);
         use Int64_VehicleAssignmentCost_Maps.Formal_Model;
      begin

         while Has_Element (Result.Vehicle_Assignments, I) loop
            pragma Assert
              (Contains (Assignment.Vehicle_Assignments, Key (Result.Vehicle_Assignments, I))
                 and then Element (Assignment.Vehicle_Assignments, Key (Result.Vehicle_Assignments, I)) = Element (Result.Vehicle_Assignments, I));
            pragma Assert
              (Contains_Corresponding_TaskOption
                 (Automation_Request,
                  TaskPlanOptions_Map,
                  Element (Assignment.Vehicle_Assignments, Key (Result.Vehicle_Assignments, I)).Last_TaskOption,
                  Key (Result.Vehicle_Assignments, I)));

            Equal_TaskOpt_Lemma (Automation_Request,
                                 TaskPlanOptions_Map,
                                 Element (Assignment.Vehicle_Assignments, Key (Result.Vehicle_Assignments, I)).Last_TaskOption,
                                 Element (Result.Vehicle_Assignments, I).Last_TaskOption,
                                 Key (Result.Vehicle_Assignments, I));

            pragma Loop_Invariant (Has_Element (Result.Vehicle_Assignments, I));
            pragma Loop_Invariant
              (for all K in 1 .. Int64_VehicleAssignmentCost_Maps_P.Get (Positions (Result.Vehicle_Assignments), I) =>
                 (Contains_Corresponding_TaskOption
                    (Result.Assignment_Sequence,
                     Element (Result.Vehicle_Assignments,
                              Int64_VehicleAssignmentCost_Maps_K.Get (Keys (Result.Vehicle_Assignments), K)).Last_TaskOption)
                    and then
                  (Contains_Corresponding_TaskOption
                     (Automation_Request,
                      TaskPlanOptions_Map,
                      Element (Result.Vehicle_Assignments,
                               Int64_VehicleAssignmentCost_Maps_K.Get (Keys (Result.Vehicle_Assignments), K)).Last_TaskOption,
                      Int64_VehicleAssignmentCost_Maps_K.Get (Keys (Result.Vehicle_Assignments), K)))));
            Next (Result.Vehicle_Assignments, I);
         end loop;

         for J in TO_Sequences.First .. Last (Result.Assignment_Sequence) loop
            if J /= Last (Result.Assignment_Sequence) then
               pragma Assert (Get (Result.Assignment_Sequence, J) = Get (Assignment.Assignment_Sequence, J));
               pragma Assert (Get (Result.Assignment_Sequence, J).TaskID in 0 .. 99_999
                                and then Get (Result.Assignment_Sequence, J).OptionID in 0 .. 99_999);
            else
               pragma Assert (Get (Result.Assignment_Sequence, J).TaskID = TaskOpt.TaskID
                                and then Get (Result.Assignment_Sequence, J).OptionID = TaskOpt.OptionID);
               pragma Assert (Get (Result.Assignment_Sequence, J).TaskID in 0 .. 99_999
                                and then Get (Result.Assignment_Sequence, J).OptionID in 0 .. 99_999);
            end if;

            pragma Loop_Invariant
              (for all K in TO_Sequences.First .. J =>
                 (Get (Result.Assignment_Sequence, K).TaskID in 0 .. 99_999
                  and then Get (Result.Assignment_Sequence, K).OptionID in 0 .. 99_999));
         end loop;
      end Prove_Initial_Value_Is_Valid;
   begin
      --  The assignment sequence is the enclosing assignment sequence with
      --  the new TaskAssignment added at the end.
      pragma Assume (Length (Assignment.Assignment_Sequence) < Count_Type'Last);
      Result.Assignment_Sequence :=
        Add (Assignment.Assignment_Sequence,
             (TaskOpt.TaskID,
              TaskOpt.OptionID,
              VehicleId,
              TimeThreshold,
              TimeTaskCompleted));

      Result.Vehicle_Assignments := Assignment.Vehicle_Assignments;
      Prove_Initial_Value_Is_Valid;
      pragma Assert (Valid_Assignment (Result, TaskPlanOptions_Map, Automation_Request));
      declare
         VAC : VehicleAssignmentCost := (TimeTaskCompleted, TaskOpt);
      begin
         pragma Assert
           (for some TaskId of TaskPlanOptions_Map =>
              (declare
                 Options : TaskOption_Seq renames Get (TaskPlanOptions_Map, TaskId).Options;
               begin
                 Contains (Options, TO_Sequences.First, Last (Options), VAC.Last_TaskOption)
                   and then Is_Eligible (Automation_Request, VAC.Last_TaskOption, VehicleId)));

         if Contains (Result.Vehicle_Assignments, VehicleId) then
            Replace (Result.Vehicle_Assignments, VehicleId, VAC);
            Prove_Final_Value_Is_Valid;
            pragma Assert (Valid_Assignment (Result, TaskPlanOptions_Map, Automation_Request));

         else
            pragma Assume (Length (Result.Vehicle_Assignments) < Count_Type'Last, "we have enough space for another vehicle");
            Insert (Result.Vehicle_Assignments, VehicleId, VAC);
            Prove_Final_Value_Is_Valid;
            pragma Assert (Valid_Assignment (Result, TaskPlanOptions_Map, Automation_Request));
         end if;
      end;

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
      Summary                : out TaskAssignmentSummary;
      Error                  : out Boolean;
      Message                : out Unbounded_String)
   is

      package Assignment_Stack is new Bounded_Stack (Assignment_Info);
      type Stack is new Assignment_Stack.Stack;
      use type Stack;

      procedure Bubble_Sort (Arr : in out Children_Arr)
      with
        Pre  =>
           Arr'Length > 0
             and then
           Valid_TaskPlanOptions (TaskPlanOptions_Map)
             and then
           (for all Child of Arr =>
              (Valid_Assignment (Child, TaskPlanOptions_Map, Automation_Request))),
        Post =>
          (for all Child of Arr =>
             (Valid_Assignment (Child, TaskPlanOptions_Map, Automation_Request)));
      --  Sorts the array of assignments in the ascending order of cost.

      procedure Equal_Implies_Valid_Assignment (A, B : Assignment_Info) with
        Ghost,
        Pre  => A = B and then Valid_TaskPlanOptions (TaskPlanOptions_Map) and then Valid_Assignment (A, TaskPlanOptions_Map, Automation_Request),
        Post => Valid_Assignment (B, TaskPlanOptions_Map, Automation_Request);

      -----------------
      -- Bubble_Sort --
      -----------------

      procedure Bubble_Sort (Arr : in out Children_Arr) is
         Switched : Boolean;
      begin
         loop
            Switched := False;
            pragma Loop_Invariant (for all Child of Arr => Valid_Assignment (Child, TaskPlanOptions_Map, Automation_Request));
            for J in Arr'First .. Arr'Last - 1 loop
               pragma Loop_Invariant (for all Child of Arr => Valid_Assignment (Child, TaskPlanOptions_Map, Automation_Request));
               if Cost (Arr (J + 1), Data.Cost_Function) < Cost (Arr (J), Data.Cost_Function) then
                  declare
                     Tmp : Assignment_Info := Arr (J + 1);
                  begin
                     Equal_Implies_Valid_Assignment (Arr (J + 1), Tmp);
                     Arr (J + 1) := Arr (J);
                     Equal_Implies_Valid_Assignment (Arr (J), Arr (J + 1));
                     Arr (J) := Tmp;
                     Equal_Implies_Valid_Assignment (Tmp, Arr (J));
                     Switched := True;
                  end;
               end if;
            end loop;

            exit when not Switched;
         end loop;
      end Bubble_Sort;

      ------------------------------------
      -- Equal_Implies_Valid_Assignment --
      ------------------------------------

      procedure Equal_Implies_Valid_Assignment (A, B : Assignment_Info) is
         I : Int64_VehicleAssignmentCost_Maps.Cursor := First (B.Vehicle_Assignments);
         use Int64_VehicleAssignmentCost_Maps.Formal_Model;
      begin

         while Has_Element (B.Vehicle_Assignments, I) loop

            pragma Assert
              (Contains (A.Vehicle_Assignments, Key (B.Vehicle_Assignments, I))
                 and then Element (A.Vehicle_Assignments, Key (B.Vehicle_Assignments, I)) = Element (B.Vehicle_Assignments, I));
            pragma Assert
               (Contains_Corresponding_TaskOption
                  (A.Assignment_Sequence,
                   Element (A.Vehicle_Assignments, Key (B.Vehicle_Assignments, I)).Last_TaskOption));
            pragma Assert
               (Contains_Corresponding_TaskOption
                  (Automation_Request,
                   TaskPlanOptions_Map,
                   Element (A.Vehicle_Assignments, Key (B.Vehicle_Assignments, I)).Last_TaskOption,
                   Key (B.Vehicle_Assignments, I)));

            Equal_TaskOpt_Lemma
               (Automation_Request,
                TaskPlanOptions_Map,
                Element (A.Vehicle_Assignments, Key (B.Vehicle_Assignments, I)).Last_TaskOption,
                Element (B.Vehicle_Assignments, I).Last_TaskOption,
                Key (B.Vehicle_Assignments, I));

            pragma Loop_Invariant (Has_Element (B.Vehicle_Assignments, I));
            pragma Loop_Invariant
              (for all K in 1 .. Int64_VehicleAssignmentCost_Maps_P.Get (Positions (B.Vehicle_Assignments), I) =>
                 (Contains_Corresponding_TaskOption
                    (B.Assignment_Sequence,
                     Element (B.Vehicle_Assignments,
                              Int64_VehicleAssignmentCost_Maps_K.Get (Keys (B.Vehicle_Assignments), K)).Last_TaskOption)
                    and then
                  (Contains_Corresponding_TaskOption
                     (Automation_Request,
                      TaskPlanOptions_Map,
                      Element (B.Vehicle_Assignments,
                               Int64_VehicleAssignmentCost_Maps_K.Get (Keys (B.Vehicle_Assignments), K)).Last_TaskOption,
                      Int64_VehicleAssignmentCost_Maps_K.Get (Keys (B.Vehicle_Assignments), K)))));
            Next (B.Vehicle_Assignments, I);
         end loop;
      end Equal_Implies_Valid_Assignment;

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
      Empty_VA_Map    : Int64_VAC_Map;
      Nodes_Visited   : Int64 := 0;
   begin
      Initialize_Algebra (Automation_Request, TaskPlanOptions_Map, Algebra, Error, Message);
      Put_Line (To_String (Message));
      if not Error then
         Put_Line ("Algebra Tree:");
         Print_Tree (Algebra);

         --  The first element is a null assignment

         pragma Assume (Size (Search_Stack) < Count_Type'Last, "we have space for another child");
         Push (Search_Stack,
                       (Empty_TA_Seq,
                        Empty_VA_Map));
         pragma Assert (for all K in 1 .. Size (Search_Stack) => Valid_Assignment (Get (Search_Stack, K), TaskPlanOptions_Map, Automation_Request));

         --  If the stack is empty, all solutions have been explored

         while Size (Search_Stack) /= 0

         --  We continue at least until we find a solution

           and then (if Min.Found
                     then (Nodes_Visited in 1 .. Data.Number_Nodes_Maximum - 1))
         loop
            pragma Loop_Invariant (for all K in 1 .. Size (Search_Stack) => Valid_Assignment (Get (Search_Stack, K), TaskPlanOptions_Map, Automation_Request));

            --  The element at the top of the stack is popped

            Pop (Search_Stack, Current_Element);

            if not Min.Found or else Cost (Current_Element, Data.Cost_Function) < Min.Cost then
               declare
                  Children_A   : Children_Arr :=
                    Children (Current_Element,
                              Algebra,
                              Automation_Request,
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
                     end if;

                     --  Else, we compute the cost for every child and push them into the
                     --  stack if their cost is lower than the current minimal cost.

                  else
                     Bubble_Sort (Children_A);
                     for J in reverse Children_A'Range loop
                        pragma Loop_Invariant (for all K in 1 .. Size (Search_Stack) => Valid_Assignment (Get (Search_Stack, K), TaskPlanOptions_Map, Automation_Request));
                        declare
                           Child : Assignment_Info := Children_A (J);
                        begin
                           if not Min.Found or else Cost (Child, Data.Cost_Function) < Min.Cost then
                              pragma Assume (Size (Search_Stack) < Count_Type'Last, "we have space for another child");
                              Push (Search_Stack, Child);
                           end if;
                        end;
                     end loop;
                     pragma Assert (for all K in 1 .. Size (Search_Stack) => Valid_Assignment (Get (Search_Stack, K), TaskPlanOptions_Map, Automation_Request));
                  end if;
               end;
               pragma Assume (Nodes_Visited < Int64'Last, "a solution is found in less than Int64'Last steps");
               Nodes_Visited := Nodes_Visited + 1;
            end if;
         end loop;

         Summary.CorrespondingAutomationRequestID := Automation_Request.RequestID;
         Summary.OperatingRegion := Automation_Request.OperatingRegion;
         Summary.TaskList := Min.Info.Assignment_Sequence;
      else
         declare
            Null_TAS : TaskAssignmentSummary;
         begin
            Summary := Null_TAS;
         end;
      end if;
      Free_Tree (Algebra);
      pragma Assert (Algebra = null);
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
      Error   : Boolean;
      Message : Unbounded_String;
   begin
      pragma Assert
        (Contains (State.m_assignmentCostMatrixes, ReqId));
      pragma Assert
        (All_Travels_In_CostMatrix
           (Element (State.m_uniqueAutomationRequests, ReqId),
            Element (State.m_taskPlanOptions, ReqId),
            Element (State.m_assignmentCostMatrixes, ReqId)));

      Run_Calculate_Assignment
        (Data,
         Element (State.m_uniqueAutomationRequests, ReqId),
         Element (State.m_assignmentCostMatrixes, ReqId),
         Element (State.m_taskPlanOptions, ReqId),
         Summary,
         Error,
         Message);

      if not Error then
         sendBroadcastMessage (Mailbox, Summary);
      else
         sendErrorMessage (Mailbox, Message);
      end if;
      Delete (State.m_assignmentCostMatrixes, ReqId);
      Delete (State.m_taskPlanOptions, ReqId);
      Delete (State.m_uniqueAutomationRequests, ReqId);
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

   ----------------------
   -- Valid_Assignment --
   ----------------------

   function Valid_Assignment
     (Assignment          : Assignment_Info;
      TaskPlanOptions_Map : Int64_TPO_Map;
      Automation_Request  : UniqueAutomationRequest)
      return Boolean
   is
     ((for all TaskAssignment of Assignment.Assignment_Sequence =>
        (TaskAssignment.TaskID in 0 .. 99_999
           and then
         TaskAssignment.OptionID in 0 .. 99_999))
       and then
     (for all EntityId of Assignment.Vehicle_Assignments =>
        (Contains_Corresponding_TaskOption
           (Assignment.Assignment_Sequence,
            Element (Assignment.Vehicle_Assignments, EntityId).Last_TaskOption)
          and then
        (Contains_Corresponding_TaskOption
           (Automation_Request,
            TaskPlanOptions_Map,
            Element (Assignment.Vehicle_Assignments, EntityId).Last_TaskOption,
            EntityId)))));

end Assignment_Tree_Branch_Bound;
