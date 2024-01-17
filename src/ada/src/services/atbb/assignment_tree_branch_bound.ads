with SPARK.Containers.Formal.Unbounded_Hashed_Maps;
with SPARK.Containers.Functional.Maps;
with Ada.Containers;                             use Ada.Containers;
with Ada.Strings.Unbounded;                      use Ada.Strings.Unbounded;
with Assignment_Tree_Branch_Bound_Communication; use Assignment_Tree_Branch_Bound_Communication;
with Common;                                     use Common;
with LMCP_Messages;                              use LMCP_Messages;

package Assignment_Tree_Branch_Bound with SPARK_Mode is

   type Cost_Function_Kind is (Minmax, Cumulative);

   package Int64_UAR_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
       (Key_Type     => Int64,
        Element_Type => UniqueAutomationRequest,
        Hash         => Int64_Hash);
   subtype Int64_UniqueAutomationRequest_Map is
     Int64_UAR_Maps.Map (Int64_UAR_Maps.Default_Modulus (10));
   use Int64_UAR_Maps;

   package Int64_TaskPlanOptions_Maps is new SPARK.Containers.Functional.Maps
     (Key_Type     => Int64,
      Element_Type => TaskPlanOptions);
   type Int64_TPO_Map is new Int64_TaskPlanOptions_Maps.Map;

   package Int64_TPO_Map_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
     (Key_Type => Int64,
      Element_Type => Int64_TPO_Map,
      Hash  => Int64_Hash);
   subtype Int64_TaskPlanOptions_Map_Map is
     Int64_TPO_Map_Maps.Map (Int64_TPO_Map_Maps.Default_Modulus (10));
   use Int64_TPO_Map_Maps;
   --use Int64_TPO_Map_Maps.Formal_Model;
   package Int64_TaskPlanOptions_Map_Maps_P renames Int64_TPO_Map_Maps.Formal_Model.P;
   package Int64_TaskPlanOptions_Map_Maps_K renames Int64_TPO_Map_Maps.Formal_Model.K;

   package Int64_ACM_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
       (Key_Type     => Int64,
        Element_Type => AssignmentCostMatrix,
        Hash         => Int64_Hash);
   subtype Int64_AssignmentCostMatrix_Map is
     Int64_ACM_Maps.Map (Int64_ACM_Maps.Default_Modulus (10));
   use Int64_ACM_Maps;
   --use Int64_ACM_Maps.Formal_Model;
   package Int64_AssignmentCostMatrix_Maps_P renames Int64_ACM_Maps.Formal_Model.P;
   package Int64_AssignmentCostMatrix_Maps_K renames Int64_ACM_Maps.Formal_Model.K;

   ----------------------------
   -- Annotation subprograms --
   ----------------------------

   function Valid_TaskPlanOptions
     (TaskPlanOptions_Map : Int64_TPO_Map)
      return Boolean with Post => True;

   function Valid_AssignmentCostMatrix
     (Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Boolean with Post => True;

   function Travel_In_CostMatrix
     (VehicleId              : Int64;
      DestOption             : TaskOption;
      Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Boolean with Post => True;

   function Travel_In_CostMatrix
     (VehicleId              : Int64;
      InitOption, DestOption : TaskOption;
      Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Boolean with Post => True;

   function All_Travels_In_CostMatrix
     (Request             : UniqueAutomationRequest;
      TaskPlanOptions_Map : Int64_TPO_Map;
      Matrix              : AssignmentCostMatrix)
      return Boolean with Post => True;

   function All_EligibleEntities_In_EntityList
     (Request             : UniqueAutomationRequest;
      TaskPlanOptions_Map : Int64_TPO_Map)
      return Boolean with Post => True;

   ----------------------------------------
   -- Assignment Tree Branch Bound types --
   ----------------------------------------

   type Assignment_Tree_Branch_Bound_Configuration_Data is record
      Cost_Function        : Cost_Function_Kind := Minmax;
      Number_Nodes_Maximum : Int64 := 0;
   end record
     with Predicate => Number_Nodes_Maximum >= 0;

   type Assignment_Tree_Branch_Bound_State is record
      m_uniqueAutomationRequests : Int64_UniqueAutomationRequest_Map;
      m_taskPlanOptions          : Int64_TaskPlanOptions_Map_Map;
      m_assignmentCostMatrixes   : Int64_AssignmentCostMatrix_Map;
   end record with
     Predicate =>
       (for all ReqId of m_taskPlanOptions =>
          (Valid_TaskPlanOptions (Element (m_taskPlanOptions, ReqId))
             and then
           Contains (m_uniqueAutomationRequests, ReqId)
             and then
           All_EligibleEntities_In_EntityList
            (Element (m_uniqueAutomationRequests, ReqId),
             Element (m_taskPlanOptions, ReqId))))
          and then
       (for all ReqId of m_assignmentCostMatrixes =>
          Valid_AssignmentCostMatrix (Element (m_assignmentCostMatrixes, ReqId))
            and then
          Contains (m_uniqueAutomationRequests, ReqId)
            and then
          Contains (m_taskPlanOptions, ReqId)
            and then
          All_Travels_In_CostMatrix
            (Element (m_uniqueAutomationRequests, ReqId),
             Element (m_taskPlanOptions, ReqId),
             Element (m_assignmentCostMatrixes, ReqId)));

   ---------------------------
   -- Service functionality --
   ---------------------------

   procedure Handle_Unique_Automation_Request
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      Areq    : UniqueAutomationRequest)
   with
     Pre =>
       not Contains (State.m_uniqueAutomationRequests, Areq.RequestID)
         and then
       not Contains (State.m_assignmentCostMatrixes, Areq.RequestID);

   procedure Handle_Task_Plan_Options
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      Options : TaskPlanOptions)
   with
     Pre =>
       (for all TaskOption of Options.Options =>
          (TaskOption.Cost >= 0 and then Options.TaskID = TaskOption.TaskID))
         and then not Contains (State.m_assignmentCostMatrixes, Options.CorrespondingAutomationRequestID)
         and then
           (not Contains (State.m_taskPlanOptions, Options.CorrespondingAutomationRequestID)
              or else
            not Has_Key (Element (State.m_taskPlanOptions, Options.CorrespondingAutomationRequestID), Options.TaskID))
         and then Contains (State.m_uniqueAutomationRequests, Options.CorrespondingAutomationRequestID)
         and then
           (for all Option of Options.Options =>
              (for all EntityId of Option.EligibleEntities =>
                 Contains (Element (State.m_uniqueAutomationRequests, Options.CorrespondingAutomationRequestID).EntityList,
                           TO_Sequences.First,
                           Last (Element (State.m_uniqueAutomationRequests, Options.CorrespondingAutomationRequestID).EntityList),
                           EntityId)));

   procedure Handle_Assignment_Cost_Matrix
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      Matrix  : AssignmentCostMatrix)
   with Pre =>
     not Contains (State.m_assignmentCostMatrixes, Matrix.CorrespondingAutomationRequestID)
       and then Valid_AssignmentCostMatrix (Matrix)
       and then Contains (State.m_uniqueAutomationRequests, Matrix.CorrespondingAutomationRequestID)
       and then Contains (State.m_taskPlanOptions, Matrix.CorrespondingAutomationRequestID)
       and then All_Travels_In_CostMatrix (Element (State.m_uniqueAutomationRequests, Matrix.CorrespondingAutomationRequestID),
                                           Element (State.m_taskPlanOptions, Matrix.CorrespondingAutomationRequestID),
                                           Matrix);

   procedure Check_Assignment_Ready
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      ReqId   : Int64);

   procedure Send_TaskAssignmentSummary
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      ReqId   : Int64)
   with
     Pre =>
       Contains (State.m_uniqueAutomationRequests, ReqId)
         and then Contains (State.m_assignmentCostMatrixes, ReqId)
         and then Contains (State.m_taskPlanOptions, ReqId)
         and then
           (for all TaskId of Element (State.m_uniqueAutomationRequests, ReqId).TaskList =>
              Has_Key (Element (State.m_taskPlanOptions, ReqId), TaskId))
         and then
           Valid_TaskPlanOptions (Element (State.m_taskPlanOptions, ReqId));

   procedure Run_Calculate_Assignment
     (Data                   : Assignment_Tree_Branch_Bound_Configuration_Data;
      Automation_Request     : UniqueAutomationRequest;
      Assignment_Cost_Matrix : AssignmentCostMatrix;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Summary                : out TaskAssignmentSummary;
      Error                  : out Boolean;
      Message                : out Unbounded_String)
   with
     Pre =>
       Valid_AssignmentCostMatrix (Assignment_Cost_Matrix)
         and then
       Valid_TaskPlanOptions (TaskPlanOptions_Map)
         and then
       (for all TaskId of Automation_Request.TaskList =>
          Has_Key (TaskPlanOptions_Map, TaskId))
         and then
       (for all Id of TaskPlanOptions_Map =>
          (for all TaskOption of Get (TaskPlanOptions_Map, Id).Options => TaskOption.TaskID = Id))
         and then
       All_Travels_In_CostMatrix (Automation_Request, TaskPlanOptions_Map, Assignment_Cost_Matrix)
         and then
       All_EligibleEntities_In_EntityList (Automation_Request, TaskPlanOptions_Map);
   --  Returns the assignment that minimizes the cost.

private

   function Valid_TaskPlanOptions
     (TaskPlanOptions_Map : Int64_TPO_Map)
      return Boolean
   is
      (for all TaskId of TaskPlanOptions_Map =>
         (TaskId in 0 .. 99_999
            and then
          TaskId = Get (TaskPlanOptions_Map, TaskId).TaskID
            and then
          (for all TaskOption of Get (TaskPlanOptions_Map, TaskId).Options =>
                  (TaskId = TaskOption.TaskID
                     and then TaskOption.OptionID in 0 .. 99_999
                     and then TaskOption.Cost >= 0))));

   function Valid_AssignmentCostMatrix
     (Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Boolean
   is
      (for all TOC of Assignment_Cost_Matrix.CostMatrix => TOC.TimeToGo >= 0);

   function Travel_In_CostMatrix
     (VehicleId              : Int64;
      DestOption             : TaskOption;
      Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Boolean
   is
     (for some TOC of Assignment_Cost_Matrix.CostMatrix =>
        (VehicleId = TOC.VehicleID
           and then 0 = TOC.InitialTaskID
           and then 0 = TOC.InitialTaskOption
           and then DestOption.TaskID = TOC.DestinationTaskID
           and then DestOption.OptionID = TOC.DestinationTaskOption));

   function Travel_In_CostMatrix
     (VehicleId              : Int64;
      InitOption, DestOption : TaskOption;
      Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Boolean
   is
     (for some TOC of Assignment_Cost_Matrix.CostMatrix =>
        (VehicleId = TOC.VehicleID
           and then InitOption.TaskID = TOC.InitialTaskID
           and then InitOption.OptionID = TOC.InitialTaskOption
           and then DestOption.TaskID = TOC.DestinationTaskID
           and then DestOption.OptionID = TOC.DestinationTaskOption));

   function Is_Eligible (Request : UniqueAutomationRequest; Option : TaskOption; VehicleId : Int64) return Boolean is
      (Contains (Request.EntityList, TO_Sequences.First, Last (Request.EntityList), VehicleId)
         and then
       (if Length (Option.EligibleEntities) > 0 then Contains (Option.EligibleEntities, TO_Sequences.First, Last (Option.EligibleEntities), VehicleId)));

   function All_Travels_In_CostMatrix
     (Request             : UniqueAutomationRequest;
      TaskPlanOptions_Map : Int64_TPO_Map;
      Matrix              : AssignmentCostMatrix)
      return Boolean
   is
     (for all VehicleId of Request.EntityList =>
        (for all TaskId_1 of TaskPlanOptions_Map =>
           (for all Option_1 of Get (TaskPlanOptions_Map, TaskId_1).Options =>
               (if Is_Eligible (Request, Option_1, VehicleId)
                then
                   Travel_In_CostMatrix (VehicleId, Option_1, Matrix)
                     and then
                   (for all TaskId_2 of TaskPlanOptions_Map =>
                      (for all Option_2 of Get (TaskPlanOptions_Map, TaskId_2).Options =>
                         (if Option_1 /= Option_2 and then Is_Eligible (Request, Option_2, VehicleId)
                          then
                             Travel_In_CostMatrix (VehicleId,
                                                   Option_1,
                                                   Option_2,
                                                   Matrix))))))));

   function All_EligibleEntities_In_EntityList
     (Request             : UniqueAutomationRequest;
      TaskPlanOptions_Map : Int64_TPO_Map)
      return Boolean
   is
     (for all TaskId of TaskPlanOptions_Map =>
        (for all Option of Get (TaskPlanOptions_Map, TaskId).Options =>
           (for all EntityId of Option.EligibleEntities =>
              Contains (Request.EntityList, TO_Sequences.First, Last (Request.EntityList), EntityId))));

end Assignment_Tree_Branch_Bound;
